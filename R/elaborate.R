# segment name: elaborate_logic ---

#' Elaborate a Motion Trace
#' @description The \code{motion_trace} equivalent of \code{dplyr::mutate()}.
#'   Adds or modifies columns while preserving the \code{motion_trace} class
#'   and all attached attributes (metadata, quality log). Optionally groups
#'   rows before mutating and ungroups after, enabling within-segment or
#'   within-band calculations.
#' @param .data A \code{motion_trace} object.
#' @param ... Column expressions passed to \code{dplyr::mutate()}.
#' @param by Character vector of column names to group by before mutating.
#'   Rows are ungrouped after the mutation. Common uses:
#'   \code{by = 'designation'} for within-segment calculations, or
#'   \code{by = c('designation', 'allocation_1_velocity_band')} for
#'   within-band calculations.
#' @param log_expr Logical; if \code{TRUE}, the deparsed text of each column
#'   expression is stored in the quality log for this call. Defaults to
#'   \code{FALSE}.
#' @return The input \code{motion_trace} with new or modified columns,
#'   class and attributes intact.
#' @export
elaborate <- function(.data, ..., by = NULL, log_expr = FALSE) {

  validate_motion_trace(.data, 'elaborate')
  cols_before <- names(.data)
  rows_before <- nrow(.data)
  existing_groups <- dplyr::group_vars(.data)

  # Capture expressions before evaluation (needed for both expression logging
  # and for passing to dplyr::mutate() via !!! below)
  quos <- rlang::enquos(...)

  # Stash attributes — dplyr::mutate drops custom ones
  meta <- attr(.data, 'metadata')
  qual <- attr(.data, 'quality')

  # Combine pre-existing groups with any explicit by= groups
  effective_by <- union(existing_groups, by)

  if (length(effective_by) > 0) {
    missing_by <- setdiff(effective_by, cols_before)
    if (length(missing_by) > 0) {
      stop(
        "elaborate(): column(s) not found for 'by' grouping: ",
        paste(missing_by, collapse = ', ')
      )
    }
    out <- .data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(effective_by))) |>
      dplyr::mutate(!!!quos) |>
      # Restore original grouping state — ungroup fully if input was ungrouped,
      # re-group to original vars if input was already grouped
      (\(x) if (length(existing_groups) > 0)
          dplyr::group_by(x, dplyr::across(dplyr::all_of(existing_groups)))
        else
          dplyr::ungroup(x))()
  } else {
    out <- dplyr::mutate(.data, !!!quos)
  }

  cols_after <- names(out)
  added <- setdiff(cols_after, cols_before)
  modified <- Filter(
    function(col) !identical(.data[[col]], out[[col]]),
    intersect(cols_before, cols_after)
  )

  # Warn on reserved column overwrites
  reserved <- c(
    'unix_time', 'x', 'y', 'z', 'f_x', 'f_y',
    'lat', 'lng', 'altitude', 'is_interpolated',
    'distance', 'velocity', 'acceleration', 'heading', 'angular_velocity',
    'jerk', 'lateral_g',
    'designation'
  )
  overwritten <- intersect(modified, reserved)
  if (length(overwritten) > 0) {
    warning(
      "elaborate(): overwriting reserved column(s): ",
      paste(overwritten, collapse = ', '),
      ". This may affect downstream pipeline steps."
    )
  }

  # Restore class
  class(out) <- c('motion_trace', setdiff(class(out), 'motion_trace'))

  # Quality log — accumulate calls so multiple elaborate() steps are all visible
  if (is.null(qual)) qual <- list()
  if (is.null(qual$elaborate)) qual$elaborate <- list(calls = list())

  dplyr_version <- tryCatch(
    as.character(utils::packageVersion('dplyr')),
    error = function(e) 'unknown'
  )

  n <- length(qual$elaborate$calls) + 1L
  qual$elaborate$calls[[as.character(n)]] <- list(
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),
    by              = effective_by,
    added           = added,
    modified        = modified,
    rows_before     = rows_before,
    rows_after      = nrow(out),
    cols_before     = cols_before,
    cols_after      = names(out),
    expressions     = if (log_expr) {
      vapply(quos, rlang::quo_text, character(1))
    } else {
      NULL
    },
    dependencies    = list(dplyr = dplyr_version)
  )

  attr(out, 'quality') <- qual

  # Metadata
  if (!is.null(meta)) {
    if (is.null(meta$elaborated_columns)) meta$elaborated_columns <- character(0)
    meta$elaborated_columns <- unique(c(meta$elaborated_columns, added))
    attr(out, 'metadata') <- meta
  }

  out
}
