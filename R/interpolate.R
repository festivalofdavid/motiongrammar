# segment name: interpolate ---

#' Repair timeline gaps
#' 
#' @description 
#' Identifies missing frames based on expected frame rate and fills them
#' 
#' @param .data A motion_trace object.
#' @param method String; 'spline', 'linear', or 'constant'.
#' @param hz Numeric; the recording frequency (e.g., 1, 10, 18). Default 1.
#' @param max_gap_frames Integer; number of missing frames to allow before interpolation.
#' 
#' @export
interpolate <- function(.data, 
                        method = 'spline', 
                        hz = 1,
                        max_gap_frames = 1){

  # Calculate expected interval in seconds
  # e.g., 10Hz = 0.1s interval (due to working with unixtime)
  interval <- 1 / hz

  # time stamping work
  output <- .data |>
    dplyr::mutate(
      unix_time_orig = unix_time,
      unix_time = round(unix_time * hz) / hz
    )

  # this is our dense grid-- so gapless
  full_seq <- data.frame(
    unix_time = seq(min(output$unix_time, na.rm = TRUE), 
                    max(output$unix_time, na.rm = TRUE), 
                    by = interval)
  )
  
  # join the two, and flag so we know which data points have been interpolated
  output <- output |>
    dplyr::full_join(full_seq, by = 'unix_time') |>
    dplyr::arrange(unix_time) |>
    dplyr::mutate(is_interpolated = is.na(unix_time_orig))

  # apply interpolation with one of the zoo methods
  output <- switch(method,
    'spline'   = output |> dplyr::mutate(x = zoo::na.spline(x, na.rm = FALSE),
                                         y = zoo::na.spline(y, na.rm = FALSE)),
    'linear'   = output |> dplyr::mutate(x = zoo::na.approx(x, na.rm = FALSE),
                                         y = zoo::na.approx(y, na.rm = FALSE)),
    'constant' = output |> dplyr::mutate(x = zoo::na.locf(x, na.rm = FALSE),
                                         y = zoo::na.locf(y, na.rm = FALSE)),
    stop("Invalid method. Choose 'spline', 'linear', or 'constant'.")
  )

  # tidy up
  output <- output |> 
    dplyr::select(-unix_time_orig)
  
  if (!inherits(output, 'motion_trace')) {
    class(output) <- c('motion_trace', class(output))
  }
  
  return(output)
}