# segment name: interp_quality_log ---

#' Quality log for the interpolate step
#'
#' Records the interpolation method, gap structure, and how many rows
#' were inserted or filled. Appends to the quality attribute.
#'
#' @param output A motion_trace object (post-interpolation).
#' @param method Character; interpolation algorithm used.
#' @param hz Numeric; sampling frequency.
#' @param max_gap_frames Integer; maximum gap size that was filled.
#' @param n_input_rows Integer; rows in the original data.
#' @param n_duplicates_removed Integer; rows lost to timestamp deduplication.
#' @param n_grid_rows Integer; rows in the dense time grid.
#' @param n_time_gaps Integer; rows inserted to fill time gaps.
#' @param n_coord_na Integer; rows that had NA coordinates from upstream QC.
#' @param n_filled_x Integer; x NAs that were successfully interpolated.
#' @param n_filled_y Integer; y NAs that were successfully interpolated.
#' @param n_filled_z Integer; z NAs that were successfully interpolated.
#' @param n_remaining_na_x Integer; x NAs still remaining after interpolation.
#' @param n_remaining_na_y Integer; y NAs still remaining after interpolation.
#' @param gap_lengths Integer vector; lengths of each consecutive NA run (pre-interpolation).
#' @param passthrough_na_counts Named integer vector; for each non-coordinate column present
#'   in the input, the number of NAs introduced in time-gap rows inserted by grid expansion.
#' @param passthrough_na_counts_after Named integer vector; NAs remaining per passthrough
#'   column after interpolation (numeric columns will have fewer than before; non-numeric
#'   columns will match the introduced count).
#' @param cols_before Character vector; column names of the input before interpolation.
#' @param numeric_passthrough Character vector; non-coordinate columns that are numeric and
#'   were interpolated alongside coordinates.
#' @param non_numeric_passthrough Character vector; non-coordinate columns that are
#'   character/factor and were left as-is (NAs remain for inserted time-gap rows).
#' @return The motion_trace object with updated quality attribute.
#' @keywords internal
interp_quality_log <- function(output, method, hz, max_gap_frames,
                                n_input_rows, n_duplicates_removed,
                                n_grid_rows, n_time_gaps, n_coord_na,
                                n_filled_x, n_filled_y, n_filled_z,
                                n_remaining_na_x, n_remaining_na_y,
                                gap_lengths, passthrough_na_counts = integer(0),
                                passthrough_na_counts_after = integer(0),
                                cols_before = NULL,
                                numeric_passthrough = character(0),
                                non_numeric_passthrough = character(0)) {

  qual <- attr(output, 'quality')
  if (is.null(qual)) qual <- list()

  # Method description for reproducibility
  method_detail <- switch(method,
    'linear'   = list(
      algorithm = 'linear',
      zoo_fn    = 'zoo::na.approx',
      description = 'Piecewise linear interpolation between valid neighbours'
    ),
    'spline'   = list(
      algorithm = 'cubic_spline',
      zoo_fn    = 'zoo::na.spline',
      description = 'Natural cubic spline interpolation through valid neighbours'
    ),
    'constant' = list(
      algorithm = 'locf',
      zoo_fn    = 'zoo::na.locf',
      description = 'Last observation carried forward (zero-order hold)'
    )
  )

  # Gap structure summary
  gap_summary <- if (length(gap_lengths) > 0) {
    list(
      n_gaps         = length(gap_lengths),
      total_missing  = sum(gap_lengths),
      min_gap        = min(gap_lengths),
      max_gap        = max(gap_lengths),
      mean_gap       = round(mean(gap_lengths), 2),
      median_gap     = stats::median(gap_lengths),
      gaps_exceeding_maxgap = sum(gap_lengths > max_gap_frames)
    )
  } else {
    list(n_gaps = 0L, total_missing = 0L)
  }

  # Duration and expected vs actual row counts
  time_range <- range(output$unix_time, na.rm = TRUE)
  duration_sec <- diff(time_range)
  expected_rows <- as.integer(duration_sec * hz) + 1L

  # Package dependencies
  interp_deps <- c('zoo', 'dplyr')
  dep_versions <- vapply(interp_deps, function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "unknown")
  }, character(1))

  n_interpolated <- sum(output$is_interpolated, na.rm = TRUE)

  # ---- Issue detection ----
  pct_interpolated <- round(n_interpolated / nrow(output) * 100, 2)
  issues <- character(0)

  if (pct_interpolated > 50) {
    issues <- c(issues, paste0('High interpolation ratio: ', pct_interpolated,
                               '% of rows are interpolated'))
  }
  if (length(gap_lengths) > 0 && max(gap_lengths) > max_gap_frames) {
    n_unfilled <- sum(gap_lengths > max_gap_frames)
    issues <- c(issues, paste0(n_unfilled, ' gap(s) exceeded max_gap_frames (',
                               max_gap_frames, ') and were left as NA'))
  }
  if (n_remaining_na_x > 0 || n_remaining_na_y > 0) {
    issues <- c(issues, paste0('Remaining NAs after interpolation: x=',
                               n_remaining_na_x, ', y=', n_remaining_na_y))
  }
  if (n_duplicates_removed > 0) {
    issues <- c(issues, paste0(n_duplicates_removed,
                               ' duplicate timestamps removed during grid rounding'))
  }
  if (abs(nrow(output) - expected_rows) > 1) {
    issues <- c(issues, paste0('Row count mismatch: expected ', expected_rows,
                               ' rows at ', hz, 'Hz but got ', nrow(output)))
  }
  if (length(passthrough_na_counts) > 0 && n_time_gaps > 0) {
    affected <- names(passthrough_na_counts)[passthrough_na_counts > 0]
    if (length(affected) > 0) {
      issues <- c(issues, paste0(
        length(affected), ' passthrough column(s) have NAs for ', n_time_gaps,
        ' inserted time-gap row(s): ', paste(affected, collapse = ', ')
      ))
    }
  }

  entry <- list(
    step            = "interpolate",
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),

    # Method
    method     = method_detail,
    parameters = list(
      hz              = hz,
      max_gap_frames  = max_gap_frames,
      interval_sec    = 1 / hz
    ),

    # Input summary
    input_rows          = n_input_rows,
    duplicates_removed  = n_duplicates_removed,
    coord_na_from_upstream = n_coord_na,

    # Grid expansion
    grid_rows       = n_grid_rows,
    time_gaps_added = n_time_gaps,
    duration_sec    = as.numeric(duration_sec),
    expected_rows   = expected_rows,

    # Gap structure (pre-interpolation)
    gaps = gap_summary,

    # Row / column provenance
    rows_before  = n_input_rows,
    rows_after   = nrow(output),
    cols_before  = if (!is.null(cols_before)) cols_before else character(0),
    cols_after   = names(output),

    # Interpolation results
    total_rows       = nrow(output),
    n_interpolated   = n_interpolated,
    pct_interpolated = pct_interpolated,
    filled = list(x = n_filled_x, y = n_filled_y, z = n_filled_z),
    remaining_na = list(x = n_remaining_na_x, y = n_remaining_na_y),

    # Passthrough columns — numeric ones are interpolated; non-numeric are left as-is
    passthrough_columns = if (length(passthrough_na_counts) > 0) {
      list(
        note = paste0(
          "x/y/z and numeric passthrough columns are interpolated. ",
          "Non-numeric columns (character, factor) are carried through as-is: ",
          n_time_gaps, " row(s) inserted to fill time gaps receive NA for non-numeric columns."
        ),
        interpolated        = numeric_passthrough,
        skipped_non_numeric = non_numeric_passthrough,
        na_counts_from_gaps = as.list(passthrough_na_counts),
        na_counts_remaining = as.list(passthrough_na_counts_after)
      )
    } else {
      list(
        note                = "No passthrough columns present.",
        interpolated        = character(0),
        skipped_non_numeric = character(0),
        na_counts_from_gaps = list(),
        na_counts_remaining = list()
      )
    },

    # Issues
    issues = if (length(issues) > 0) issues else NULL,

    # Dependencies
    dependencies = setNames(dep_versions, interp_deps)
  )

  qual$interpolate <- append(qual$interpolate, list(entry))
  attr(output, 'quality') <- qual

  # ---- Update metadata ----
  meta <- attr(output, 'metadata')
  if (is.null(meta)) meta <- list()
  meta$interpolation_method <- method
  meta$interpolation_hz     <- hz
  meta$interpolation_applied <- TRUE
  attr(output, 'metadata') <- meta

  output
}

# segment name: interpolate ---

#' Repair timeline gaps
#'
#' @description
#' Identifies missing frames based on expected frame rate and fills them
#' using linear, spline, or constant (LOCF) interpolation.
#'
#' @param .data A motion_trace object.
#' @param method String; 'linear' (default), 'spline', or 'constant'.
#' @param hz Numeric; the recording frequency (e.g., 1, 10, 18). When \code{NULL}
#'   (default), the value is read from \code{meta$native_hz} set by \code{initiate()}.
#'   Falls back to 1 Hz with a message if not available. Pass an explicit value to
#'   override.
#' @param max_gap_frames Integer; gaps larger than this (in frames) are left as NA.
#'
#' @return An interpolated \code{motion_trace} object.
#' @export
interpolate <- function(.data,
                        method = 'linear',
                        hz = NULL,
                        max_gap_frames = 5){

  validate_motion_trace(.data, 'interpolate')

  if (nrow(.data) == 0) {
    message("interpolate(): input has 0 rows — returning unchanged.")
    return(.data)
  }

  n_input_rows <- nrow(.data)
  cols_before <- names(.data)

  # Resolve hz: prefer explicit arg, then metadata, then fall back to 1
  if (is.null(hz)) {
    meta_hz <- attr(.data, 'metadata')$native_hz
    if (!is.null(meta_hz) && !is.na(meta_hz) && meta_hz > 0) {
      hz <- meta_hz
      message(sprintf('interpolate(): using hz = %g from metadata (native_hz). Pass hz explicitly to override.', hz))
    } else {
      hz <- 1
      message('interpolate(): hz not specified and not found in metadata — defaulting to hz = 1. If your data has a higher sample rate, pass hz explicitly (e.g. hz = 10).')
    }
  }

  # Calculate expected interval in seconds
  # e.g., 10Hz = 0.1s interval (due to working with unixtime)
  interval <- 1 / hz

  # time stamping work -- round to the grid
  output <- .data |>
    dplyr::mutate(
      unix_time_orig = unix_time,
      unix_time = round(unix_time * hz) / hz
    )

  # Rounding can collapse multiple rows to the same timestamp.
  # Keep only the first occurrence to avoid duplicate keys.
  n_before_dedup <- nrow(output)
  output <- output |>
    dplyr::filter(!duplicated(unix_time))
  n_duplicates_removed <- n_before_dedup - nrow(output)

  # Capture passthrough columns before grid expansion.
  # Numeric non-coordinate columns (e.g. HeartRate) are interpolated alongside x/y/z.
  # Non-numeric columns (character, factor) are carried through as-is — zoo functions
  # cannot handle them and will crash with a cryptic vector-type error if attempted.
  interp_coord_cols    <- c('x', 'y', 'z', 'unix_time', 'unix_time_orig', 'is_interpolated')
  passthrough_cols     <- setdiff(names(output), interp_coord_cols)
  numeric_passthrough  <- passthrough_cols[
    vapply(passthrough_cols, function(col) is.numeric(output[[col]]), logical(1))
  ]
  non_numeric_passthrough <- setdiff(passthrough_cols, numeric_passthrough)

  # this is our dense grid-- so gapless.
  # Round the seq() output to the same hz-grid precision as the input timestamps
  # to prevent IEEE 754 drift causing join misses (e.g. 0.1 + 0.1 ≠ 0.2 exactly).
  full_seq <- data.frame(
    unix_time = round(
      seq(min(output$unix_time, na.rm = TRUE),
          max(output$unix_time, na.rm = TRUE),
          by = interval) * hz
    ) / hz
  )
  n_grid_rows <- nrow(full_seq)

  # track which rows already had NA coordinates (from coordinate QC)
  # before we join the time grid -- these are NOT time gaps.
  # Store as a temporary column so row order after arrange() doesn't matter.
  has_z <- 'z' %in% names(output)
  output$.coord_was_na <- is.na(output$x) | is.na(output$y) | (has_z & is.na(output$z))
  n_coord_na <- sum(output$.coord_was_na)

  # join the two, and flag so we know which data points have been interpolated
  output <- output |>
    dplyr::full_join(full_seq, by = 'unix_time') |>
    dplyr::arrange(unix_time)

  n_time_gaps <- sum(is.na(output$unix_time_orig))

  # Count NAs introduced in inserted rows per passthrough column.
  # Inserted rows are those where unix_time_orig is NA (they came from full_seq, not the input).
  if (n_time_gaps > 0L && length(passthrough_cols) > 0L) {
    inserted_mask <- is.na(output$unix_time_orig)
    passthrough_na_counts <- vapply(passthrough_cols, function(col) {
      sum(is.na(output[[col]][inserted_mask]))
    }, integer(1))
  } else {
    passthrough_na_counts <- setNames(integer(length(passthrough_cols)), passthrough_cols)
  }

  # Extend the coord_was_na flag to cover newly inserted rows.
  # Original rows use the per-row column set before the join; inserted rows are FALSE.
  coord_was_na_full <- !is.na(output$.coord_was_na) & output$.coord_was_na
  output$.coord_was_na <- NULL

  # track which rows *needed* interpolation (time-gap inserts or upstream NA coords)
  needed_interp <- is.na(output$unix_time_orig) | coord_was_na_full

  # Measure gap structure before interpolation (consecutive NA runs in x)
  x_na_pre <- is.na(output$x)
  gap_lengths <- rle(x_na_pre)
  gap_lengths <- gap_lengths$lengths[gap_lengths$values]

  # Count NAs before interpolation
  na_x_before <- sum(is.na(output$x))
  na_y_before <- sum(is.na(output$y))
  na_z_before <- if (has_z) sum(is.na(output$z)) else 0L

  # apply interpolation with one of the zoo methods
  # linear is safest; spline can fabricate wild values in gaps
  output <- switch(method,
    'spline'   = output |> dplyr::mutate(x = zoo::na.spline(x, na.rm = FALSE, maxgap = max_gap_frames),
                                         y = zoo::na.spline(y, na.rm = FALSE, maxgap = max_gap_frames),
                                         z = if(has_z) zoo::na.spline(z, na.rm = FALSE, maxgap = max_gap_frames) else z),
    'linear'   = output |> dplyr::mutate(x = zoo::na.approx(x, na.rm = FALSE, maxgap = max_gap_frames),
                                         y = zoo::na.approx(y, na.rm = FALSE, maxgap = max_gap_frames),
                                         z = if(has_z) zoo::na.approx(z, na.rm = FALSE, maxgap = max_gap_frames) else z),
    'constant' = output |> dplyr::mutate(x = zoo::na.locf(x, na.rm = FALSE, maxgap = max_gap_frames),
                                         y = zoo::na.locf(y, na.rm = FALSE, maxgap = max_gap_frames),
                                         z = if(has_z) zoo::na.locf(z, na.rm = FALSE, maxgap = max_gap_frames) else z),
    stop("Invalid method. Choose 'spline', 'linear', or 'constant'.")
  )

  # Interpolate numeric passthrough columns (e.g. HeartRate, Cadence) using the same method.
  # Non-numeric columns (character, factor) are intentionally skipped.
  if (length(numeric_passthrough) > 0) {
    zoo_fn <- switch(method,
      'linear'   = \(col) zoo::na.approx(col, na.rm = FALSE, maxgap = max_gap_frames),
      'spline'   = \(col) zoo::na.spline(col, na.rm = FALSE, maxgap = max_gap_frames),
      'constant' = \(col) zoo::na.locf(col, na.rm = FALSE, maxgap = max_gap_frames)
    )
    output <- output |>
      dplyr::mutate(dplyr::across(dplyr::all_of(numeric_passthrough), zoo_fn))
  }

  # Count NAs remaining after all interpolation (coordinates + numeric passthrough)
  na_x_after <- sum(is.na(output$x))
  na_y_after <- sum(is.na(output$y))
  na_z_after <- if (has_z) sum(is.na(output$z)) else 0L

  passthrough_na_counts_after <- if (length(passthrough_cols) > 0L) {
    vapply(passthrough_cols, function(col) sum(is.na(output[[col]])), integer(1))
  } else {
    setNames(integer(0), character(0))
  }

  # is_interpolated = row needed filling AND coordinates are now present
  # rows that still have NA coords (gap too large, edges, no valid lat/lng) stay FALSE
  coords_present <- !is.na(output$x) & !is.na(output$y)
  if (has_z) coords_present <- coords_present & !is.na(output$z)
  output$is_interpolated <- needed_interp & coords_present

  # tidy up
  output <- output |>
    dplyr::select(-unix_time_orig)

  if (!inherits(output, 'motion_trace')) {
    class(output) <- c('motion_trace', class(output))
  }

  output <- interp_quality_log(
    output, method, hz, max_gap_frames,
    n_input_rows       = n_input_rows,
    n_duplicates_removed = n_duplicates_removed,
    n_grid_rows        = n_grid_rows,
    n_time_gaps        = n_time_gaps,
    n_coord_na         = n_coord_na,
    n_filled_x         = na_x_before - na_x_after,
    n_filled_y         = na_y_before - na_y_after,
    n_filled_z         = na_z_before - na_z_after,
    n_remaining_na_x   = na_x_after,
    n_remaining_na_y   = na_y_after,
    gap_lengths             = gap_lengths,
    passthrough_na_counts       = passthrough_na_counts,
    passthrough_na_counts_after = passthrough_na_counts_after,
    cols_before                 = cols_before,
    numeric_passthrough         = numeric_passthrough,
    non_numeric_passthrough     = non_numeric_passthrough
  )

  return(output)
}