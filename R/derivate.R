# segment name: derivate ---

#' Calculate Derivatives of Motion
#' 
#' @description
#' Computes linear and angular velocity and acceleration. Defaults to 
#' using filtered coordinates where possible
#' 
#' @param .data A motion_trace object.
#' @param use_filtered Logical: if TRUE, uses f_x and f_y. Defaults to TRUE.
#' @param window Numeric: give the size of the window to calculate derivatives across
#' 
#' @return A \code{motion_trace} object with velocity and acceleration columns.
#' @export
#' 

derivate <- function(.data, use_filtered = TRUE, window = 5){
  
  # load in source cols
  x_col <- if (use_filtered && 'f_x' %in% names(.data)) 'f_x' else 'x'
  y_col <- if (use_filtered && 'f_y' %in% names(.data)) 'f_y' else 'y'
  
  # 2 time to derive
  output <- .data |>
    dplyr::mutate(
      # get unix time between start and end of window we set earlier
      dt = unix_time - dplyr::lag(unix_time, n = window),
      # displacements within the window
      dx = .data[[x_col]] - dplyr::lag(.data[[x_col]], n = window),
      dy = .data[[y_col]] - dplyr::lag(.data[[y_col]], n = window),
      # Calculate distance before deriving velocity
      distance = sqrt(dx^2 + dy^2),
      # Get a vector of direction/heading
      heading_rad = atan2(dy, dx),
      # Change in heading over k wndow
      rad_diff = (heading_rad - dplyr::lag(heading_rad, n = window) + pi) %% (2 * pi) - pi,
      # we don't want radians realistically  so let's just convert it to degrees
      angular_velocity = (rad_diff * (180 / pi)) / dt,
      # linear velocity in same window
      velocity = distance / dt,
      acceleration = (velocity - dplyr::lag(velocity, n = window)) / dt ) |>
    # Clean up temp columns
    dplyr::select(-dx, -dy, -rad_diff, -heading_rad) |>
    dplyr::mutate(
      dplyr::across(c(distance, velocity, angular_velocity), ~tidyr::replace_na(., 0))
    ) |> 
    dplyr::mutate(angular_velocity = abs(angular_velocity))
  
  return(output)
}