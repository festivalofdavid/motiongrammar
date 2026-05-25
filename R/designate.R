# segment name: designate_logic ---
#' designate Motion Data
#' @description Finalises segmentation using statistical masking and type-safe combining.
#' @param .data Tibble with 'velocity', 'unix_time', and 'is_interpolated'.
#' @param v_threshold Velocity (m/s) to distinguish active vs downtime.
#' @param min_pts Minimum segment length in rows. Defaults to 5 seconds
#'   worth of data based on the sample rate (Hz) in metadata, or 50 rows
#'   if Hz is unavailable.
#' @param method Character; segmentation method. \code{'corbett'} (default) uses
#'   PELT change-point detection. \code{'manual'} imports user-supplied labels
#'   from a CSV via \code{file}.
#' @param file Path to a CSV with columns \code{designation}, \code{unix_start},
#'   \code{unix_end}. Required when \code{method = 'manual'}.
#' @param cols Column mapping for the manual CSV. \code{NULL} (default) expects
#'   exact columns \code{designation}, \code{unix_start}, \code{unix_end}.
#'   A named character vector provides explicit mapping (e.g.
#'   \code{c(designation = 'label', unix_start = 'start_time', unix_end = 'end_time')}).
#'   \code{'guess'} uses fuzzy matching to auto-detect columns.
#' @export
designate <- function(.data,
  v_threshold = 0.25,
  min_pts = NULL,
  method = 'corbett',
  file = NULL,
  cols = NULL
) {

  validate_motion_trace(.data, 'designate')
  cols_before <- names(.data)
  rows_before <- nrow(.data)

  # Default min_pts from Hz (5 seconds)
  if (is.null(min_pts)) {
    meta <- attr(.data, 'metadata')
    hz <- meta$interpolation_hz %||% meta$hz %||% meta$sample_rate %||% 10L
    min_pts <- as.integer(hz * 5)
  }

  # Will be populated for corbett method changepoint details
  cp_indices <- NULL
  cp_segment_lengths <- NULL

  if(method == 'corbett'){
  # 1. Work on the finite-velocity subset -- PELT cannot handle NAs.
  finite_idx <- which(is.finite(.data$velocity))
  if (length(finite_idx) == 0) {
    stop('designate(): no finite velocity values left after filtering.')
  }
  finite_vel <- .data$velocity[finite_idx]

  # 2. Get PELT statistical bounds
  m_pelt <- changepoint::cpt.mean(
    data      = finite_vel,
    method    = 'PELT',
    penalty   = 'SIC'
  )

  # 3. Categorise by changepoint (on the finite subset)
  cp_indices <- changepoint::cpts(m_pelt)
  if (length(cp_indices) > 0) {
    seg_bounds <- c(0L, cp_indices, length(finite_vel))
    cp_segment_lengths <- as.integer(diff(seg_bounds))
  } else {
    cp_segment_lengths <- length(finite_vel)
  }
  pelt_id <- findInterval(
    seq_along(finite_vel),
    c(0, cp_indices)
  )

  sub <- dplyr::tibble(
    .orig_row = finite_idx,
    velocity  = finite_vel,
    pelt_id   = pelt_id
  ) |>
    dplyr::group_by(pelt_id) |>
    dplyr::mutate(
      is_downtime = mean(velocity, na.rm = TRUE) < v_threshold
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      type_group_id = dplyr::consecutive_id(is_downtime)
    )

  # 4. Combine tiny segments together
  sub <- sub |>
    dplyr::group_by(type_group_id) |>
    dplyr::mutate(
      segment_size = dplyr::n()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      refined_id = ifelse(
        segment_size < min_pts,
        NA_real_,
        as.numeric(type_group_id)
      ),
      refined_id = zoo::na.locf(refined_id, na.rm = FALSE),
      refined_id = tidyr::replace_na(refined_id, 1)
    ) |>
    # 5. Final Semantic Labelling
    dplyr::group_by(refined_id) |>
    dplyr::mutate(
      final_mean = mean(velocity, na.rm = TRUE),
      is_static_final = final_mean < v_threshold
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label_prefix = ifelse(is_static_final, 'relief', 'active'),
      segment_order = dplyr::consecutive_id(refined_id)
    ) |>
    dplyr::group_by(label_prefix) |>
    dplyr::mutate(
      type_rank = dplyr::consecutive_id(segment_order),
      designation = paste0(label_prefix, '_', type_rank)
    ) |>
    dplyr::ungroup()

  # 6. Map designations back to the full trace (preserving all rows)
  .data$designation <- NA_character_
  .data$designation[sub$.orig_row] <- sub$designation

  # Rows with NA velocity (interpolated gaps, leading/trailing rows) have no
  # PELT designation. Propagate the nearest valid designation to fill them so
  # downstream quantitate() doesn't silently drop those rows.
  .data$designation <- zoo::na.locf(.data$designation, na.rm = FALSE)
  .data$designation <- zoo::na.locf(.data$designation, na.rm = FALSE, fromLast = TRUE)

  refined <- .data
  } else if (method == 'manual') {

    if (is.null(file)) {
      stop("designate(): 'file' must be provided when method = 'manual'.")
    }

    labels <- utils::read.csv(file, stringsAsFactors = FALSE)
    labels <- .resolve_designation_cols(labels, cols)

    # Non-equi join: assign designation where unix_start <= unix_time <= unix_end
    # NA times in either the data or the CSV would produce NA in the comparison,
    # which R's vector assignment silently ignores — use which() to be explicit.
    .data$designation <- NA_character_
    for (i in seq_len(nrow(labels))) {
      in_range <- which(
        !is.na(.data$unix_time) &
        .data$unix_time >= labels$unix_start[i] &
        .data$unix_time <= labels$unix_end[i]
      )
      .data$designation[in_range] <- labels$designation[i]
    }

    refined <- .data

  } else {
    stop("designate(): unknown method '", method, "'.")
  }

  # ── Quality log ────────────────────────────────────────────────────────────
  desig_vals   <- refined$designation
  n_total      <- nrow(refined)
  n_designated <- sum(!is.na(desig_vals))
  desig_counts <- as.list(table(desig_vals[!is.na(desig_vals)]))

  cp_version <- if (method == 'corbett') {
    tryCatch(
      as.character(utils::packageVersion('changepoint')),
      error = function(e) 'unknown'
    )
  } else {
    NA_character_
  }

  qual <- attr(refined, 'quality')
  if (is.null(qual)) qual <- list()

  entry <- list(
    step            = 'designate',
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),
    method          = method,

    algorithm = if (method == 'corbett') list(
      package         = 'changepoint',
      package_version = cp_version,
      fn              = 'cpt.mean',
      segmentation    = 'PELT',
      penalty         = 'SIC',
      v_threshold     = v_threshold,
      min_pts         = min_pts,
      n_changepoints     = length(cp_indices),
      changepoint_indices = as.integer(cp_indices),
      segment_lengths    = as.integer(cp_segment_lengths)
    ) else NULL,

    source_file  = if (method == 'manual') file else NULL,
    cols_mapping = if (method == 'manual') cols else NULL,

    rows_before    = rows_before,
    rows_after     = nrow(refined),
    cols_before    = cols_before,
    cols_after     = names(refined),

    n_total_rows   = n_total,
    n_designated   = n_designated,
    pct_designated = round(n_designated / n_total * 100, 2),
    n_segments     = length(desig_counts),
    segment_counts = desig_counts
  )

  qual$designate <- append(qual$designate, list(entry))
  attr(refined, 'quality') <- qual

  # ── Metadata ───────────────────────────────────────────────────────────────
  meta <- attr(refined, 'metadata')
  if (!is.null(meta)) {
    meta$designation_method <- method
    meta$designation_source <- if (method == 'manual') file else 'computed'
    attr(refined, 'metadata') <- meta
  }

  if (!inherits(refined, 'motion_trace')) {
    class(refined) <- c('motion_trace', class(refined))
  }

  return(refined)
}

#' Resolve Designation Column Names
#'
#' Handles all three \code{cols} modes for the manual method: \code{NULL}
#' (expect exact names), a named character vector (explicit mapping), or
#' \code{'guess'} (fuzzy auto-detection). Returns the data frame with
#' columns renamed to the canonical names.
#'
#' @param labels Data frame read from the manual CSV.
#' @param cols Column specification (NULL, named vector, or 'guess').
#' @return \code{labels} with columns renamed to \code{designation},
#'   \code{unix_start}, \code{unix_end}.
#' @keywords internal
.resolve_designation_cols <- function(labels, cols) {
  canonical <- c('designation', 'unix_start', 'unix_end')

  if (is.null(cols)) {
    # Default: expect exact column names
    missing <- setdiff(canonical, names(labels))
    if (length(missing) > 0) {
      stop(
        "designate(): CSV is missing required columns: ",
        paste(missing, collapse = ', '),
        "\nUse cols = 'guess' to auto-detect or provide an explicit mapping."
      )
    }
    return(labels)
  }

  if (is.character(cols) && length(cols) == 1 && cols == 'guess') {
    mapping <- guess_designation_cols(labels)
    for (canon in names(mapping)) {
      idx <- which(names(labels) == mapping[[canon]])
      if (length(idx) > 0) {
        names(labels)[idx[1]] <- canon
      }
    }
    return(labels)
  }

  # Named vector: explicit mapping
  if (is.character(cols) && !is.null(names(cols))) {
    missing_keys <- setdiff(canonical, names(cols))
    if (length(missing_keys) > 0) {
      stop(
        "designate(): cols mapping is missing keys: ",
        paste(missing_keys, collapse = ', '),
        "\nExpected: c(designation = '...', unix_start = '...', unix_end = '...')"
      )
    }

    for (canon in canonical) {
      csv_col <- cols[[canon]]
      idx <- which(names(labels) == csv_col)
      if (length(idx) == 0) {
        stop(
          "designate(): column '", csv_col,
          "' (mapped from '", canon,
          "') not found in CSV. Available: ",
          paste(names(labels), collapse = ', ')
        )
      }
      names(labels)[idx[1]] <- canon
    }
    return(labels)
  }

  stop("designate(): 'cols' must be NULL, 'guess', or a named character vector.")
}

#' Guess Designation Column Names
#'
#' Uses fuzzy string matching (Jaro-Winkler via \pkg{stringdist}) to
#' auto-detect which CSV columns correspond to \code{designation},
#' \code{unix_start}, and \code{unix_end}.
#'
#' @param labels Data frame to inspect.
#' @return A named character vector mapping canonical names to detected
#'   CSV column names.
#' @keywords internal
guess_designation_cols <- function(labels) {
  targets <- list(
    designation = c('designation', 'label', 'segment', 'behavior',
                    'behaviour', 'category', 'event', 'code'),
    unix_start  = c('unix_start', 'start_time', 'start', 'begin',
                    'onset', 'time_start', 'start_unix'),
    unix_end    = c('unix_end', 'end_time', 'end', 'stop',
                    'offset', 'time_end', 'end_unix')
  )

  available_cols <- names(labels)
  mappings <- character(0)

  for (target_name in names(targets)) {
    patterns <- targets[[target_name]]
    available_lower <- tolower(available_cols)

    # Exact match first
    matched <- NULL
    for (pattern in patterns) {
      exact <- which(available_lower == tolower(pattern))
      if (length(exact) > 0) {
        matched <- available_cols[exact[1]]
        break
      }
    }

    # Fuzzy match if no exact match
    if (is.null(matched)) {
      best_score <- 0
      for (pattern in patterns) {
        for (i in seq_along(available_cols)) {
          sim <- 1 - stringdist::stringdist(
            tolower(pattern), available_lower[i], method = 'jw'
          )
          if (sim > best_score && sim >= 0.8) {
            best_score <- sim
            matched <- available_cols[i]
          }
        }
      }
    }

    if (is.null(matched)) {
      stop(
        "designate(): could not detect a column for '", target_name,
        "' in CSV. Available columns: ",
        paste(available_cols, collapse = ', '),
        "\nProvide an explicit cols mapping instead."
      )
    }

    message(sprintf("  %s -> '%s'", target_name, matched))
    mappings[[target_name]] <- matched

    # Remove from pool to prevent double-matching
    available_cols <- setdiff(available_cols, matched)
  }

  mappings
}

#' Export Designation Ranges
#' @description Summarises a \code{designation} column into start/end time ranges.
#'   Optionally writes the result to CSV.
#' @param .data A data frame with \code{designation} and \code{unix_time} columns.
#' @param path Optional file path to write the CSV. If \code{NULL} (default),
#'   returns the summary data frame without writing.
#' @return A data frame with columns \code{designation}, \code{unix_start},
#'   \code{unix_end}, returned invisibly when \code{path} is provided.
#' @export
export_designations <- function(.data, path = NULL) {

  if (!'designation' %in% names(.data)) {
    stop("export_designations(): .data must contain a 'designation' column.")
  }

  # Identify contiguous runs so repeated labels aren't collapsed
  .data$`.run_id` <- dplyr::consecutive_id(.data$designation)

  summary_df <- .data |>
    dplyr::group_by(`.run_id`, designation) |>
    dplyr::summarise(
      unix_start = if (any(!is.na(unix_time))) min(unix_time, na.rm = TRUE) else NA_real_,
      unix_end   = if (any(!is.na(unix_time))) max(unix_time, na.rm = TRUE) else NA_real_,
      .groups = 'drop'
    ) |>
    dplyr::arrange(unix_start) |>
    dplyr::select(designation, unix_start, unix_end)

  if (!is.null(path)) {
    utils::write.csv(summary_df, file = path, row.names = FALSE)
    return(invisible(summary_df))
  }

  summary_df
}