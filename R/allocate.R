# segment name: allocate_logic ---

#' Allocate Intensity via Named Allocations
#' @description Assigns data points to intensity bands using named allocation
#'   thresholds. Call multiple times with different \code{allocation_name}
#'   values to layer several allocation schemes onto the same data, or use
#'   \code{\link{allocate_csv}} to apply multiple profiles from a single file.
#' @param .data Tibble with \code{velocity}, \code{acceleration}, and/or
#'   \code{angular_velocity} columns.
#' @param allocation A named list of numeric breakpoint vectors. Recognised
#'   keys: \code{velocity}, \code{acceleration}, \code{angular_velocity}.
#'   Defaults to a standard GPS threshold set.
#' @param allocation_name A string used to prefix the output band columns,
#'   e.g. \code{'gps_standard'} produces \code{gps_standard_velocity_band}.
#'   Defaults to \code{'allocation_1'}.
#' @return The input tibble with additional \code{<allocation_name>_<derivative>_band}
#'   columns.
#' @export
allocate <- function(
  .data,
  allocation = list(
    velocity = c(0, 2, 4, 6, 8),
    acceleration = c(0, 1, 2, 3, 4),
    angular_velocity = c(0, 100, 200, 300, 400)
  ),
  allocation_name = 'allocation_1'
) {

  validate_motion_trace(.data, 'allocate')
  cols_before <- names(.data)
  rows_before <- nrow(.data)

  col_suffix <- list(
    velocity = 'velocity_band',
    acceleration = 'acceleration_band',
    angular_velocity = 'angular_velocity_band'
  )

  use_abs <- c('acceleration', 'angular_velocity')

  .apply_bands <- function(x, thresholds) {
    labels <- paste0('band_', seq_len(length(thresholds) - 1))
    idx <- findInterval(x, thresholds)
    idx[idx == 0 | idx > length(labels)] <- NA_integer_
    factor(labels[idx], levels = labels, ordered = TRUE)
  }

  applied <- character(0)

  for (deriv in names(allocation)) {
    if (!deriv %in% names(col_suffix)) {
      warning(sprintf("allocate(): unknown derivative '%s', skipping.", deriv))
      next
    }
    if (!deriv %in% names(.data)) {
      warning(sprintf("allocate(): column '%s' not found in data, skipping.", deriv))
      next
    }

    vals <- if (deriv %in% use_abs) abs(.data[[deriv]]) else .data[[deriv]]
    out_col <- paste0(allocation_name, '_', col_suffix[[deriv]])
    .data[[out_col]] <- .apply_bands(vals, allocation[[deriv]])
    applied <- c(applied, deriv)
  }

  # ── Quality log ────────────────────────────────────────────────────────────
  band_detail <- lapply(applied, function(deriv) {
    thresholds <- allocation[[deriv]]
    ranges <- vapply(seq_len(length(thresholds) - 1), function(i) {
      sprintf('[%g, %g)', thresholds[i], thresholds[i + 1])
    }, character(1))
    list(n_bands = length(thresholds) - 1L, thresholds = thresholds, ranges = ranges)
  })
  names(band_detail) <- applied

  qual <- attr(.data, 'quality')
  if (is.null(qual)) qual <- list()
  if (is.null(qual$allocate)) qual$allocate <- list(allocations = list())

  qual$allocate$allocations[[allocation_name]] <- list(
    allocation_name = allocation_name,
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),
    source          = 'manual',
    source_file     = NULL,
    rows_before     = rows_before,
    rows_after      = nrow(.data),
    cols_before     = cols_before,
    cols_after      = names(.data),
    bands           = band_detail,
    algorithm       = list(
      fn = 'findInterval',
      package = 'base',
      output_type = 'ordered factor',
      out_of_range = 'NA (values outside band range are not assigned)',
      abs_applied = 'acceleration, angular_velocity (absolute value taken before banding)'
    )
  )

  attr(.data, 'quality') <- qual

  # ── Metadata ───────────────────────────────────────────────────────────────
  meta <- attr(.data, 'metadata')
  if (!is.null(meta)) {
    if (is.null(meta$allocations_applied)) meta$allocations_applied <- character(0)
    meta$allocations_applied <- c(meta$allocations_applied, allocation_name)
    attr(.data, 'metadata') <- meta
  }

  .data
}

#' Allocate Intensity from a CSV Profile Document
#' @description Reads a CSV that defines one or more named allocation profiles
#'   and applies all of them to the data. Multiple profiles in a single file
#'   are each applied in turn via \code{\link{allocate}}.
#' @param .data Tibble with \code{velocity}, \code{acceleration}, and/or
#'   \code{angular_velocity} columns.
#' @param file Path to a CSV with columns \code{profile_name},
#'   \code{target_derivative}, \code{profile_start}, \code{profile_end}.
#' @details Each row in the CSV describes one band within a profile:
#'   \itemize{
#'     \item \code{profile_name} – identifier for the allocation set
#'       (e.g. \code{gps_standard}).
#'     \item \code{target_derivative} – one of \code{velocity},
#'       \code{acceleration}, or \code{angular_velocity}.
#'     \item \code{profile_start} – lower bound of the band (inclusive).
#'     \item \code{profile_end} – upper bound of the band.
#'   }
#'   Bands are ordered by \code{profile_start} and numbered accordingly.
#'   Output column naming follows the same
#'   \code{<profile_name>_<derivative>_band} convention as \code{allocate}.
#' @return The input tibble with additional band columns for every profile
#'   defined in the CSV.
#' @export
allocate_csv <- function(.data, file) {

  profiles <- utils::read.csv(file, stringsAsFactors = FALSE)

  required <- c('profile_name', 'target_derivative', 'profile_start', 'profile_end')
  missing <- setdiff(required, names(profiles))
  if (length(missing) > 0) {
    stop(
      "allocate_csv(): CSV is missing required columns: ",
      paste(missing, collapse = ', ')
    )
  }

  for (pname in unique(profiles$profile_name)) {
    p_rows <- profiles[profiles$profile_name == pname, ]
    allocation <- list()

    for (deriv in unique(p_rows$target_derivative)) {
      d_rows <- p_rows[p_rows$target_derivative == deriv, ]
      d_rows <- d_rows[order(d_rows$profile_start), ]
      allocation[[deriv]] <- d_rows$profile_start
    }

    .data <- allocate(
      .data,
      allocation = allocation,
      allocation_name = pname
    )

    # Patch quality log to record the CSV source for this profile
    qual <- attr(.data, 'quality')
    qual$allocate$allocations[[pname]]$source <- 'csv'
    qual$allocate$allocations[[pname]]$source_file <- file
    attr(.data, 'quality') <- qual
  }

  .data
}
