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

  # load in source cols -- only use filtered if columns exist AND have real data
  x_col <- 'x'
  y_col <- 'y'
  if (use_filtered && 'f_x' %in% names(.data) && 'f_y' %in% names(.data)) {
    if (sum(!is.na(.data[['f_x']])) > 0 && sum(!is.na(.data[['f_y']])) > 0) {
      x_col <- 'f_x'
      y_col <- 'f_y'
    }
  }

  # 2 time to derive
  output <- .data |>
    dplyr::mutate(
      # get unix time between start and end of window we set earlier
      dt = unix_time - dplyr::lag(unix_time, n = window),
      # guard against dt == 0 (collapsed timestamps) to prevent Inf
      dt = dplyr::if_else(dt == 0 | is.na(dt), NA_real_, dt),
      # single-frame time step (for angular velocity which uses 1-row lag)
      dt1 = unix_time - dplyr::lag(unix_time, n = 1),
      dt1 = dplyr::if_else(dt1 == 0 | is.na(dt1), NA_real_, dt1),
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
      # Angular velocity using dot product method (degrees/sec)
      # uses 1-row lag on velocity vectors, so divide by single-frame dt
      vx_lag = dplyr::lag(vx),
      vy_lag = dplyr::lag(vy),
      speed_prev = sqrt(vx_lag^2 + vy_lag^2),
      speed_curr = sqrt(vx^2 + vy^2),
      dot_prod = vx_lag * vx + vy_lag * vy,
      cos_angle = dplyr::if_else(
        speed_prev > 0 & speed_curr > 0,
        pmax(pmin(dot_prod / (speed_prev * speed_curr), 1), -1),
        NA_real_
      ),
      angular_velocity = (acos(cos_angle) / dt1) * (180 / pi),
      # acceleration
      acceleration = (velocity - dplyr::lag(velocity, n = window)) / dt
    ) |>
    # Clean up temp columns
    dplyr::select(-dx, -dy, -dt1, -vx, -vy, -vx_lag, -vy_lag, -speed_prev, -speed_curr, -dot_prod, -cos_angle) |>
    dplyr::mutate(
      dplyr::across(c(distance, velocity, angular_velocity), ~tidyr::replace_na(., 0))
    ) |>
    dplyr::mutate(
      angular_velocity = dplyr::if_else(is.nan(angular_velocity), 0, angular_velocity)
    )

  return(output)
}