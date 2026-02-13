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
      # linear velocity in same window
      velocity = distance / dt,
      # velocity components (needed for angular velocity)
      vx = dx / dt,
      vy = dy / dt,
      # heading  
      heading = atan2(vy, vx) * 180 / pi,
      # Angular velocity using cross product method
      vx_lag = dplyr::lag(vx),
      vy_lag = dplyr::lag(vy),
      cross_z = vx_lag * vy - vy_lag * vx,
      dot_prod = vx_lag * vx + vy_lag * vy,
      angular_velocity = atan2(cross_z, dot_prod) / dt, # radians sec
      angular_velocity = (atan2(cross_z, dot_prod) / dt) * (180 / pi),  # degrees/sec #
      # acceleration
      acceleration = (velocity - dplyr::lag(velocity, n = window)) / dt
    ) |>
    # Clean up temp columns
    dplyr::select(-dx, -dy, -vx, -vy, -vx_lag, -vy_lag, -cross_z, -dot_prod) |>
    dplyr::mutate(
      dplyr::across(c(distance, velocity, angular_velocity), ~tidyr::replace_na(., 0))
    ) |> 
    dplyr::mutate(angular_velocity = abs(angular_velocity))
  
  return(output)
}