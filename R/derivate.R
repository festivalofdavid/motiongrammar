# segment name: derivate ---

#' Calculate Derivatives of Motion
#'
#' @param .data A motion_trace object.
#' @param use_filtered Logical: if TRUE, uses f_x and f_y. Defaults to TRUE.
#' @param window Numeric: size of the window (in rows) to calculate *linear* derivatives across.
#' @param speed_floor Numeric: speeds below this (m/s) are treated as stationary for heading/omega.
#'
#' @return A motion_trace with distance, velocity, acceleration, heading, angular_velocity.
#' @export
derivate <- function(.data, use_filtered = TRUE, window = 5, speed_floor = 0.5){

  # Choose columns
  x_col <- "x"
  y_col <- "y"
  if (use_filtered && all(c("f_x", "f_y") %in% names(.data))) {
    if (sum(!is.na(.data[["f_x"]])) > 0 && sum(!is.na(.data[["f_y"]])) > 0) {
      x_col <- "f_x"
      y_col <- "f_y"
    }
  }

  out <- .data |>
    dplyr::arrange(unix_time) |>
    dplyr::mutate(
      # dt definitions
      dt  = unix_time - dplyr::lag(unix_time, n = window),
      dt  = dplyr::if_else(dt <= 0 | is.na(dt), NA_real_, dt),

      dt1 = unix_time - dplyr::lag(unix_time, n = 1),
      dt1 = dplyr::if_else(dt1 <= 0 | is.na(dt1), NA_real_, dt1),

      # ---- LINEAR (window-based, as you had it) ----
      dx = .data[[x_col]] - dplyr::lag(.data[[x_col]], n = window),
      dy = .data[[y_col]] - dplyr::lag(.data[[y_col]], n = window),

      distance = sqrt(dx^2 + dy^2),
      velocity = distance / dt,

      # ---- INSTANTANEOUS velocity for HEADING/OMEGA ----
      dx1 = .data[[x_col]] - dplyr::lag(.data[[x_col]], n = 1),
      dy1 = .data[[y_col]] - dplyr::lag(.data[[y_col]], n = 1),

      vx1 = dx1 / dt1,
      vy1 = dy1 / dt1,
      speed1 = sqrt(vx1^2 + vy1^2),

      # heading in radians ([-pi, pi])
      heading_rad = dplyr::if_else(speed1 >= speed_floor, atan2(vy1, vx1), NA_real_),
      heading = heading_rad * 180 / pi,

      # wrapped delta heading (robust to +-pi wrap)
      dtheta = heading_rad - dplyr::lag(heading_rad, 1),
      dtheta = atan2(sin(dtheta), cos(dtheta)),

      # angular velocity (rad/s then deg/s)
      angular_velocity = (dtheta / dt1) * (180 / pi),

      # ---- ACCELERATION (keep your approach; uses window velocity) ----
      acceleration = (velocity - dplyr::lag(velocity, n = window)) / dt
    ) |>
    dplyr::select(-dx, -dy, -dx1, -dy1, -vx1, -vy1, -speed1, -heading_rad, -dtheta) |>
    dplyr::mutate(
      dplyr::across(c(distance, velocity), ~ tidyr::replace_na(., 0)),
      angular_velocity = tidyr::replace_na(angular_velocity, 0),
      angular_velocity = dplyr::if_else(!is.finite(angular_velocity), 0, angular_velocity)
    )

  out
}
