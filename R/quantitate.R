# segment name: quantitate_logic ---

#' Quantitate Motion Grammar
#' @description Aggregates motion data by designation and one or more intensity
#'   band dimensions. Three scopes control what rows are included and how the
#'   designation column is used in grouping.
#'
#' @param .data Tibble output from \code{allocate()}.
#' @param allocation Character; the allocation name used in \code{allocate()}
#'   (e.g. \code{'allocation_1'}).
#' @param derivative Character; one or more derivatives whose bands to group
#'   by. Any combination of \code{'velocity'}, \code{'acceleration'},
#'   \code{'angular_velocity'}.
#' @param scope Character; controls grouping and row inclusion:
#'   \describe{
#'     \item{\code{'designation'}}{(default) Groups by \code{designation} and
#'       bands. Use \code{active_only} to optionally restrict to active
#'       segments.}
#'     \item{\code{'session'}}{Groups by bands only, across the whole session.
#'       Restricts to active designations (excludes relief).}
#'     \item{\code{'active_session'}}{Groups by bands only, across the whole
#'       session. Includes all rows (active and relief).}
#'   }
#' @param active_only Logical; when \code{scope = 'designation'}, restricts
#'   rows to segments whose \code{designation} starts with \code{'active_'}.
#'   Ignored for other scopes. Defaults to \code{FALSE}.
#' @param ... Summarise expressions passed to \code{dplyr::summarise()}.
#' @return A summarised tibble grouped by the resolved band columns and,
#'   when \code{scope = 'designation'}, \code{designation}.
#' @export
quantitate <- function(
  .data,
  allocation  = 'allocation_1',
  derivative  = 'velocity',
  scope       = 'designation',
  active_only = FALSE,
  ...
) {

  scope <- match.arg(scope, c('designation', 'session', 'active_session'))

  suffix_map <- c(
    velocity         = 'velocity_band',
    acceleration     = 'acceleration_band',
    angular_velocity = 'angularvelocity_band'
  )

  unknown <- setdiff(derivative, names(suffix_map))
  if (length(unknown) > 0) {
    stop(
      "quantitate(): unknown derivative(s): ",
      paste(unknown, collapse = ', '),
      ". Must be one or more of: ",
      paste(names(suffix_map), collapse = ', ')
    )
  }

  band_cols <- paste0(allocation, '_', suffix_map[derivative])

  missing <- setdiff(band_cols, names(.data))
  if (length(missing) > 0) {
    stop(
      "quantitate(): band column(s) not found in data: ",
      paste(missing, collapse = ', '),
      ". Run allocate(allocation_name = '", allocation, "') first."
    )
  }

  # Row filtering
  if (scope == 'session' || (scope == 'designation' && active_only)) {
    .data <- dplyr::filter(.data, grepl('^active_', designation))
  }

  # Grouping
  group_vars <- if (scope == 'designation') c('designation', band_cols) else band_cols

  .data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(..., .groups = 'drop')
}

#' Time in Bands
#' @description Convenience wrapper around \code{\link{quantitate}} that
#'   computes the duration spent in each band. Requires interpolated data so
#'   that each row represents a fixed time interval (derived from the
#'   \code{interpolation_hz} field in metadata).
#' @param .data Tibble output from \code{allocate()}.
#' @param allocation Character; allocation name (e.g. \code{'allocation_1'}).
#' @param derivative Character; one or more derivatives to group by.
#' @param scope Character; \code{'designation'}, \code{'session'}, or
#'   \code{'active_session'}. See \code{\link{quantitate}}.
#' @param active_only Logical; passed to \code{\link{quantitate}} when
#'   \code{scope = 'designation'}.
#' @return A summarised tibble with a \code{duration_sec} column.
#' @export
band_duration <- function(
  .data,
  allocation  = 'allocation_1',
  derivative  = 'velocity',
  scope       = 'designation',
  active_only = FALSE
) {

  meta     <- attr(.data, 'metadata')
  hz       <- meta$interpolation_hz %||% meta$hz %||% meta$sample_rate %||% 10L
  interval <- 1 / hz

  quantitate(
    dplyr::mutate(.data, .row_dur = interval),
    allocation  = allocation,
    derivative  = derivative,
    scope       = scope,
    active_only = active_only,
    duration_sec = sum(.row_dur, na.rm = TRUE)
  )
}
