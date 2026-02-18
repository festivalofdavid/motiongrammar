# segment name: derivate ---

#' Calculate Derivatives of Motion
#'
#' @param .data A motion_trace object.
#' @param use_filtered Logical: if TRUE, uses f_x and f_y. Defaults to TRUE.
#' @param window Window size in rows. Defaults to the sample rate in Hz
#'   (i.e. 1-second periods). Pass a single integer to use the same window
#'   for all derivatives, or a named list to set per-derivative windows:
#'   \code{list(velocity = 5, acceleration = 10, angular_velocity = 8)}.
#'   Any derivative not named in the list falls back to the default.
#' @param speed_floor Numeric: speeds below this (m/s) are treated as stationary for heading/omega.
#' @param signed_omega Logical: if TRUE, angular velocity retains sign (positive = left/CCW,
#'   negative = right/CW). Defaults to FALSE (absolute magnitude).
#'
#' @return A motion_trace with distance, velocity, acceleration, heading, angular_velocity.
#' @export
derivate <- function(.data, use_filtered = TRUE, window = NULL, speed_floor = 0.5, signed_omega = FALSE){

  validate_motion_trace(.data, 'derivate')

  # Default window = sample rate (1-second periods)
  meta <- attr(.data, 'metadata')
  default_window <- meta$interpolation_hz %||% meta$hz %||% meta$sample_rate %||% 5L
  default_window <- as.integer(max(default_window, 1L))

  # Resolve per-derivative windows
  if (is.null(window)) {
    w_vel <- default_window
    w_ang <- default_window
    w_acc <- default_window
  } else if (is.list(window)) {
    w_vel <- as.integer(window$velocity %||% window$distance %||% default_window)
    w_ang <- as.integer(window$angular_velocity %||% default_window)
    w_acc <- as.integer(window$acceleration %||% default_window)
  } else {
    w_vel <- as.integer(window)
    w_ang <- w_vel
    w_acc <- w_vel
  }

  # Choose columns
  x_col <- "x"
  y_col <- "y"
  used_filtered <- FALSE

  if (use_filtered) {
    if (all(c("f_x", "f_y") %in% names(.data)) &&
        sum(!is.na(.data[["f_x"]])) > 0 &&
        sum(!is.na(.data[["f_y"]])) > 0) {
      x_col <- "f_x"
      y_col <- "f_y"
      used_filtered <- TRUE
    } else {
      message('filtrate() has not been run -- derivate() is using unfiltered coordinates (x, y).')
    }
  }

  # ---- VELOCITY / DISTANCE / HEADING ----
  out <- .data |>
    dplyr::arrange(unix_time) |>
    dplyr::mutate(
      .dt_vel = unix_time - dplyr::lag(unix_time, n = w_vel),
      .dt_vel = dplyr::if_else(.dt_vel <= 0 | is.na(.dt_vel), NA_real_, .dt_vel),
      .dx_vel = .data[[x_col]] - dplyr::lag(.data[[x_col]], n = w_vel),
      .dy_vel = .data[[y_col]] - dplyr::lag(.data[[y_col]], n = w_vel),
      distance = sqrt(.dx_vel^2 + .dy_vel^2),
      velocity = distance / .dt_vel,
      heading  = dplyr::if_else(
        velocity >= speed_floor,
        atan2(.dy_vel, .dx_vel) * (180 / pi),
        NA_real_
      )
    )

  # ---- ANGULAR VELOCITY (heading finite difference) ----
  out <- out |>
    dplyr::mutate(
      .dt_ang = unix_time - dplyr::lag(unix_time, n = w_ang),
      .dt_ang = dplyr::if_else(.dt_ang <= 0 | is.na(.dt_ang), NA_real_, .dt_ang),
      .dh     = heading - dplyr::lag(heading, n = w_ang),
      .dh     = ((.dh + 180) %% 360) - 180,
      angular_velocity = if (signed_omega) .dh / .dt_ang else abs(.dh / .dt_ang)
    )

  # ---- ACCELERATION ----
  out <- out |>
    dplyr::mutate(
      .dt_acc = unix_time - dplyr::lag(unix_time, n = w_acc),
      .dt_acc = dplyr::if_else(.dt_acc <= 0 | is.na(.dt_acc), NA_real_, .dt_acc),
      acceleration = (velocity - dplyr::lag(velocity, n = w_acc)) / .dt_acc
    )

  # Clean up temporary columns and fill NAs
  out <- out |>
    dplyr::select(-dplyr::starts_with('.')) |>
    dplyr::mutate(
      dplyr::across(c(distance, velocity), ~ tidyr::replace_na(., 0)),
      angular_velocity = tidyr::replace_na(angular_velocity, 0),
      angular_velocity = dplyr::if_else(!is.finite(angular_velocity), 0, angular_velocity)
    )

  # ── Quality log ────────────────────────────────────────────────────────────
  col_summary <- function(x) {
    x_fin <- x[is.finite(x)]
    list(
      n_valid  = length(x_fin),
      n_na     = sum(is.na(x)),
      min      = if (length(x_fin) > 0) min(x_fin) else NA_real_,
      max      = if (length(x_fin) > 0) max(x_fin) else NA_real_
    )
  }

  windows_used <- list(velocity = w_vel, angular_velocity = w_ang, acceleration = w_acc)
  window_default <- meta$interpolation_hz %||% meta$hz %||% meta$sample_rate %||% 5L

  issues <- character(0)
  if (!used_filtered && use_filtered) {
    issues <- c(issues, 'Filtered coordinates not available; fell back to raw (x, y).')
  }

  qual <- attr(out, 'quality')
  if (is.null(qual)) qual <- list()

  dep_versions <- vapply(c('dplyr', 'tidyr'), function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) 'unknown')
  }, character(1))

  qual$derivate <- list(
    step      = 'derivate',
    timestamp = Sys.time(),

    coordinate_source = list(
      requested_filtered = use_filtered,
      used_filtered      = used_filtered,
      x_col              = x_col,
      y_col              = y_col
    ),

    parameters = list(
      window_default   = as.integer(window_default),
      velocity         = w_vel,
      acceleration     = w_acc,
      angular_velocity = w_ang,
      speed_floor      = speed_floor,
      signed_omega     = signed_omega
    ),

    algorithms = list(
      distance_velocity = list(
        method  = 'Euclidean finite difference',
        formula = 'sqrt((x[i] - x[i-w])^2 + (y[i] - y[i-w])^2) / (t[i] - t[i-w])',
        window  = w_vel
      ),
      heading = list(
        method      = 'Four-quadrant arctangent',
        formula     = 'atan2(dy, dx) * (180 / pi)',
        speed_floor = speed_floor,
        note        = 'NA assigned when velocity < speed_floor'
      ),
      angular_velocity = list(
        method  = 'Heading finite difference with circular wrapping',
        formula = '((heading[i] - heading[i-w] + 180) %% 360 - 180) / dt',
        window  = w_ang,
        signed  = signed_omega,
        note    = if (signed_omega) 'Signed: positive = CCW, negative = CW'
                  else 'Unsigned: absolute magnitude'
      ),
      acceleration = list(
        method  = 'Velocity finite difference',
        formula = '(velocity[i] - velocity[i-w]) / (t[i] - t[i-w])',
        window  = w_acc
      )
    ),

    outputs = list(
      distance         = col_summary(out$distance),
      velocity         = col_summary(out$velocity),
      acceleration     = col_summary(out$acceleration),
      heading          = col_summary(out$heading),
      angular_velocity = col_summary(out$angular_velocity)
    ),

    dependencies = setNames(dep_versions, c('dplyr', 'tidyr')),
    issues = issues
  )

  attr(out, 'quality') <- qual

  # ── Metadata ───────────────────────────────────────────────────────────────
  meta$derivate_source   <- if (used_filtered) 'filtered (f_x, f_y)' else 'raw (x, y)'
  meta$derivate_windows  <- windows_used
  attr(out, 'metadata') <- meta

  out
}
