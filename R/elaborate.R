# segment name: elaborate_logic ---

# Walk an rlang expression AST and return a character vector of any
# max()/min() calls that receive >=2 column-name symbols as arguments.
# These are almost always mistaken non-vectorised reductions; the user
# likely wants pmax()/pmin() for element-wise row operations.
.find_multi_col_reductions <- function(expr, cols) {
  reduction_fns <- c('max', 'min')
  found <- character(0)
  if (!rlang::is_call(expr)) return(found)

  fn_name <- tryCatch(as.character(rlang::call_name(expr)), error = function(e) '')
  if (fn_name %in% reduction_fns) {
    args      <- rlang::call_args(expr)
    col_args  <- Filter(function(a) rlang::is_symbol(a) && as.character(a) %in% cols, args)
    if (length(col_args) >= 2L) {
      col_nms <- vapply(col_args, as.character, character(1))
      found   <- c(found, sprintf('%s(%s)', fn_name, paste(col_nms, collapse = ', ')))
    }
  }

  for (arg in rlang::call_args(expr)) {
    found <- c(found, .find_multi_col_reductions(arg, cols))
  }
  found
}

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

  # Detect accidental non-vectorised reductions: max(x, y) in mutate() returns
  # one scalar for the whole column, not an element-wise result. Users almost
  # always want pmax(x, y) here. Fire the warning before evaluation so the
  # user can fix it before bad data reaches the quality log.
  reduction_hits <- unlist(lapply(quos, function(q) {
    .find_multi_col_reductions(rlang::get_expr(q), cols_before)
  }), use.names = FALSE)
  if (length(reduction_hits) > 0L) {
    warning(
      "elaborate(): possible non-vectorised reduction detected. ",
      "In dplyr::mutate(), max(a, b) computes one scalar across the whole column, not row-wise. ",
      "Use pmax(a, b) / pmin(a, b) for element-wise operations. ",
      "Detected: ", paste(unique(reduction_hits), collapse = '; '),
      call. = FALSE
    )
  }

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

  # Restore class — unique() preserves the existing S3 layout (e.g. grouped_df)
  # rather than destructively rebuilding the class vector from scratch.
  class(out) <- unique(c('motion_trace', class(out)))

  # Quality log — each call to elaborate() appends one entry to qual$elaborate,
  # consistent with the flat-list pattern used by all other pipeline steps.
  if (is.null(qual)) qual <- list()

  dplyr_version <- tryCatch(
    as.character(utils::packageVersion('dplyr')),
    error = function(e) 'unknown'
  )

  entry <- list(
    step                 = 'elaborate',
    step_id              = .mg_step_id(),
    package_version      = .mg_pkg_version(),
    timestamp            = .mg_timestamp(),
    by                   = effective_by,
    added                = added,
    modified             = modified,
    overwritten_reserved = overwritten,
    rows_before          = rows_before,
    rows_after           = nrow(out),
    cols_before          = cols_before,
    cols_after           = names(out),
    expressions          = if (log_expr) {
      vapply(quos, rlang::quo_text, character(1))
    } else {
      NULL
    },
    dependencies         = list(dplyr = dplyr_version)
  )

  qual$elaborate <- append(qual$elaborate, list(entry))

  attr(out, 'quality') <- qual

  # Metadata
  if (!is.null(meta)) {
    if (is.null(meta$elaborated_columns)) meta$elaborated_columns <- character(0)
    meta$elaborated_columns <- unique(c(meta$elaborated_columns, added))
    attr(out, 'metadata') <- meta
  }

  out
}
