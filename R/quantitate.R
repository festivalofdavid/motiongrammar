# segment name: quantitate_logic ---

#' Quantitate Motion Grammar
#' @description Aggregates motion data by designation and one or more intensity
#'   band dimensions. Three scopes control what rows are included and how the
#'   designation column is used in grouping. Set \code{allocation = NULL} for
#'   band-free totals.
#'
#' @param .data Tibble output from \code{allocate()}.
#' @param allocation Character; the allocation name used in \code{allocate()}
#'   (e.g. \code{'allocation_1'}). Set to \code{NULL} to skip band grouping
#'   and compute totals within designation segments (or the whole session).
#' @param derivative Character; one or more derivatives whose bands to group
#'   by. Any combination of \code{'velocity'}, \code{'acceleration'},
#'   \code{'angular_velocity'}. Ignored when \code{allocation = NULL}.
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
#' @return A summarised tibble. Grouping columns are \code{designation} (when
#'   \code{scope = 'designation'}) and/or the resolved band columns (when
#'   \code{allocation} is provided).
#' @export
quantitate <- function(
  .data,
  allocation = 'allocation_1',
  derivative = 'velocity',
  scope = 'designation',
  active_only = FALSE,
  ...
) {

  validate_motion_trace(.data, 'quantitate')
  cols_before <- names(.data)
  rows_before <- nrow(.data)
  scope <- match.arg(scope, c('designation', 'session', 'active_session'))

  suffix_map <- c(
    velocity = 'velocity_band',
    acceleration = 'acceleration_band',
    angular_velocity = 'angular_velocity_band'
  )

  if (!is.null(allocation)) {
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
  } else {
    band_cols <- character(0)
  }

  existing_groups <- dplyr::group_vars(.data)

  # Row filtering
  if (scope == 'session' || (scope == 'designation' && active_only)) {
    .data <- dplyr::filter(.data, grepl('^active_', designation))
  }

  # Grouping: designation scope always includes designation column;
  # session scopes group by bands only (empty = single-row result when NULL).
  # Pre-existing groups (e.g. from group_by()) are prepended.
  core_vars <- if (scope == 'designation') c('designation', band_cols) else band_cols
  group_vars <- union(existing_groups, core_vars)

  n_rows_in <- nrow(.data)

  result <- .data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(..., .groups = 'drop')

  attr(result, 'quantitate_log') <- list(
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),
    allocation      = allocation,
    derivative      = if (!is.null(allocation)) derivative else NULL,
    scope           = scope,
    active_only     = active_only,
    group_vars      = group_vars,
    band_cols       = if (!is.null(allocation)) band_cols else NULL,
    rows_before     = rows_before,
    rows_after      = nrow(result),
    cols_before     = cols_before,
    cols_after      = names(result),
    n_rows_in       = n_rows_in,
    n_rows_out      = nrow(result),
    dependencies    = list(
      dplyr = tryCatch(as.character(utils::packageVersion('dplyr')), error = function(e) 'unknown')
    )
  )

  result
}

#' Time in Bands
#' @description Convenience wrapper around \code{\link{quantitate}} that
#'   computes the duration spent in each band (or designation segment when
#'   \code{allocation = NULL}). Requires interpolated data so that each row
#'   represents a fixed time interval, derived from \code{interpolation_hz}
#'   in metadata.
#' @param .data Tibble output from \code{allocate()}.
#' @param allocation Character or \code{NULL}; allocation name. \code{NULL}
#'   returns duration per designation segment (or whole session).
#' @param derivative Character; one or more derivatives to group by. Ignored
#'   when \code{allocation = NULL}.
#' @param scope Character; \code{'designation'}, \code{'session'}, or
#'   \code{'active_session'}. See \code{\link{quantitate}}.
#' @param active_only Logical; passed to \code{\link{quantitate}} when
#'   \code{scope = 'designation'}.
#' @return A summarised tibble with a \code{duration_sec} column.
#' @export
band_duration <- function(
  .data,
  allocation = 'allocation_1',
  derivative = 'velocity',
  scope = 'designation',
  active_only = FALSE
) {

  meta <- attr(.data, 'metadata')
  hz <- meta$interpolation_hz %||% meta$hz %||% meta$sample_rate %||% 10L
  interval <- 1 / hz

  quantitate(
    dplyr::mutate(.data, .row_dur = interval),
    allocation = allocation,
    derivative = derivative,
    scope = scope,
    active_only = active_only,
    duration_sec = sum(.row_dur, na.rm = TRUE)
  )
}
