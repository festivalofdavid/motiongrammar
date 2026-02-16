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
#' @return The motion_trace object with updated quality attribute.
#' @keywords internal
interp_quality_log <- function(output, method, hz, max_gap_frames,
                                n_input_rows, n_duplicates_removed,
                                n_grid_rows, n_time_gaps, n_coord_na,
                                n_filled_x, n_filled_y, n_filled_z,
                                n_remaining_na_x, n_remaining_na_y,
                                gap_lengths) {

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

  qual$interpolate <- list(
    step      = "interpolate",
    timestamp = Sys.time(),

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

    # Interpolation results
    total_rows       = nrow(output),
    n_interpolated   = n_interpolated,
    pct_interpolated = round(n_interpolated / nrow(output) * 100, 2),
    filled = list(x = n_filled_x, y = n_filled_y, z = n_filled_z),
    remaining_na = list(x = n_remaining_na_x, y = n_remaining_na_y),

    # Dependencies
    dependencies = setNames(dep_versions, interp_deps)
  )

  attr(output, 'quality') <- qual
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
#' @param hz Numeric; the recording frequency (e.g., 1, 10, 18). Default 1.
#' @param max_gap_frames Integer; gaps larger than this (in frames) are left as NA.
#'
#' @return An interpolated \code{motion_trace} object.
#' @export
interpolate <- function(.data,
                        method = 'linear',
                        hz = 1,
                        max_gap_frames = 5){

  n_input_rows <- nrow(.data)

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

  # this is our dense grid-- so gapless
  full_seq <- data.frame(
    unix_time = seq(min(output$unix_time, na.rm = TRUE),
                    max(output$unix_time, na.rm = TRUE),
                    by = interval)
  )
  n_grid_rows <- nrow(full_seq)

  # track which rows already had NA coordinates (from coordinate QC)
  # before we join the time grid -- these are NOT time gaps
  has_z <- 'z' %in% names(output)
  coord_was_na <- is.na(output$x) | is.na(output$y) | (has_z & is.na(output$z))
  n_coord_na <- sum(coord_was_na)

  # join the two, and flag so we know which data points have been interpolated
  output <- output |>
    dplyr::full_join(full_seq, by = 'unix_time') |>
    dplyr::arrange(unix_time)

  n_time_gaps <- sum(is.na(output$unix_time_orig))

  # extend the coord_was_na flag to cover the newly inserted rows
  # original rows keep their flag; inserted (time-gap) rows are FALSE
  # because they are genuine gaps, not coordinate QC removals
  coord_was_na_full <- rep(FALSE, nrow(output))
  coord_was_na_full[!is.na(output$unix_time_orig)] <- coord_was_na

  # flag: TRUE for time-gap rows AND for rows where coordinate set x/y to NA
  output <- output |>
    dplyr::mutate(is_interpolated = is.na(unix_time_orig) | coord_was_na_full)

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

  # Count NAs after interpolation
  na_x_after <- sum(is.na(output$x))
  na_y_after <- sum(is.na(output$y))
  na_z_after <- if (has_z) sum(is.na(output$z)) else 0L

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
    gap_lengths        = gap_lengths
  )

  return(output)
}