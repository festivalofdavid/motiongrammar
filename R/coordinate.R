# segment name: coordinate ---

#' Convert latitude and longitude to xy (and z if we have it from altitude) metrics
#' 
#' @param from Character: usually 'latlong'
#' @param to Character: usually 'xyz'
#' @param crs Number: GPS system, defaults to most common
#' @param norm Logical or Numeric; if TRUE, anchors the trace so the first row is (0,0,0). If a numeric vector (e.g. c(x, y)), sets a custom absolute origin.
#' @param from_units Character; the units of the source ('m', 'cm', 'mm', 'km', 'ft', 'in', 'yd', 'mi').
#' @param to_units Character; the units of the target. Defaults to 'm'.
#' @param rotate NULL or a list of two c(lat, lng) vectors defining a sideline. When provided, rotates coordinates so the sideline aligns with the x-axis.
#' @return An object of class \code{motion_trace}.
#' @export
coordinate <- function(.data,
                      from = 'latlong',
                      to = 'xyz',
                      crs = 3857,
                      norm = FALSE,
                      from_units = NULL,
                      to_units = 'm',
                      rotate = NULL){
  
  # Build a list of major conversions we may need
   conversions <- list(
    # Metric
    'mm' = 1000,
    'cm' = 100,
    'm'  = 1,
    'km' = 0.001,
    # Imperial
    'in' = 39.3701,
    'ft' = 3.28084,
    'yd' = 1.09361,
    'mi' = 0.000621371)
  
  # Check entered unit is supported by me
  if (!(to_units %in% names(conversions))){
    stop('Unsupported to_units. Must be one of: ',
    paste(names(conversions),
    collapse = ', '))
  }

  if (from == 'latlong' && to == 'xyz') {
    # Make sure we actually have latitude and longitude (using strava's naming conventions)
    required_cols <- c('lat', 'lng')
    if (!all(required_cols %in% names(.data))) {
      stop('Input data must contain "lat" and "lng" columns.')
    }
    # Use sf package to deal with the spherical nature of the earth.
    coords <- .data |>
      sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326, remove = FALSE) |>
      sf::st_transform(crs = crs) |>
      sf::st_coordinates()
    
    # Add xyz to the motion data set
    # z will usually be 0, but like, just in case we get a provider that has it.
    # currently just use altitude from strava as a proxy.
    output <- .data |>
      dplyr::mutate(
        x = coords[,1],
        y = coords[,2],
        z = if ('altitude' %in% names(.data)) altitude else 0)
    
    
    # Error handling -- ensure ths is also a motion_trace file type.
    if (!inherits(output, 'motion_trace')){
      class(output) <- c('motion_trace',
                        class(output))
    }
    
  }

else if (from == 'xyz' && to == 'xyz') {
    if (!all(c('x', 'y') %in% names(.data))) {
      stop('Input data must contain "x" and "y" columns.')
    }

    if (!is.null(from_units)) {
      if (!(from_units %in% names(conversions))) {
        stop('from_units must be one of: ', paste(names(conversions), collapse = ', '))
      }
      # Logic: Convert to Metres first, then to target
      scalar_from <- conversions[[from_units]]
      scalar_to   <- conversions[[to_units]]
      total_scale <- scalar_to / scalar_from

      output <- .data |>
        dplyr::mutate(
          x = x * total_scale,
          y = y * total_scale,
          z = if ('z' %in% names(.data)) z * total_scale else 0
        )
    } else {
      # No unit conversion — passthrough for norm/rotate on existing xy data
      output <- .data
      if (!'z' %in% names(output)) {
        output <- output |> dplyr::mutate(z = 0)
      }
    }
  }
else {
    stop('Transformation path not supported. Use "latlong" to "xyz" or "xyz" to "xyz".')
  }

# --- Normalise so we don't have x vals of like 8000 unless we want them
  if (!isFALSE(norm)) {
    # If TRUE, just use first set of coordinates as origin
    if (isTRUE(norm)) {
      origin_x <- output$x[1]
      origin_y <- output$y[1]
      origin_z <- output$z[1]
    } 
    # If numeric, use the user's defined anchor. So like people could use MCG as an anchor
    else if (is.numeric(norm) && length(norm) >= 2) {
      origin_x <- norm[1]
      origin_y <- norm[2]
      origin_z <- if (length(norm) == 3) norm[3] else 0
    }

    output <- output |>
      dplyr::mutate(
        x = x - origin_x,
        y = y - origin_y,
        z = z - origin_z
      )
  }

  # --- Rotation: align sideline with x-axis ---
  if (!is.null(rotate)) {
    # Resolve sideline points
    if (!is.list(rotate) || length(rotate) != 2) {
      stop('rotate must be a list of two c(lat, lng) vectors defining a sideline.')
    }
    sideline <- rotate

    # Project the two sideline GPS points into the same CRS
    sideline_sf <- sf::st_as_sf(
      data.frame(lng = c(sideline[[1]][2], sideline[[2]][2]),
                 lat = c(sideline[[1]][1], sideline[[2]][1])),
      coords = c('lng', 'lat'), crs = 4326
    ) |> sf::st_transform(crs = crs) |> sf::st_coordinates()

    dx <- sideline_sf[2, 1] - sideline_sf[1, 1]
    dy <- sideline_sf[2, 2] - sideline_sf[1, 2]
    theta <- atan2(dy, dx)

    # Apply 2D rotation so sideline aligns with x-axis
    cos_t <- cos(-theta)
    sin_t <- sin(-theta)
    x_old <- output$x
    y_old <- output$y
    output <- output |>
      dplyr::mutate(
        x = x_old * cos_t - y_old * sin_t,
        y = x_old * sin_t + y_old * cos_t
      )

    attr(output, 'metadata')$rotation_angle <- theta
  }

  if (!inherits(output, 'motion_trace')) {
    class(output) <- c('motion_trace', class(output))
  }

  attr(output, 'metadata')$units <- to_units
  attr(output, 'metadata')$origin_type <- if (isTRUE(norm)) 'starting_pos' else if (is.numeric(norm)) 'custom' else 'none'

  return(output)
}
