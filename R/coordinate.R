# segment name: coordinate ---

#' Convert latitude and longitude to xy (and z if we have it from altitude) metrics
#' 
#' @param from Character: usually 'latlong'
#' @param to Character: usually 'xyz'
#' @param crs Number: GPS system, defaults to most common
#' @param norm Logical or Numeric; if TRUE, anchors the trace so the first row is (0,0,0). If a numeric vector (e.g. c(x, y)), sets a custom absolute origin.
#' @param from_units Character; the units of the source ('m', 'cm', 'mm', 'km', 'ft', 'in', 'yd', 'mi').
#' @param to_units Character; the units of the target. Defaults to 'm'.
#' @return An object of class \code{motion_trace}.
#' @export
coordinate <- function(.data, 
                      from = 'latlong', 
                      to = 'xyz', 
                      crs = 3857,
                      norm = FALSE,
                      from_units = NULL,
                      to_units = 'm'){
  
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

  # Make sure we actually have latitude and longitude (using strava's naming conventions)
  required_cols <- c('lat', 'lng')
  if (!all(required_cols %in% names(.data))) {
    stop('Input data must contain "lat" and "lng" columns.')
  }

  if (from == 'latlong' && to == 'xyz') {
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
    if (is.null(from_units) || !(from_units %in% names(conversions))) {
      stop('from_units must be specified as: ', paste(names(conversions), collapse = ', '))
    }
    
    # Logic: Convert to Metres first, then to target
    scalar_from <- conversions[[from_units]]
    scalar_to   <- conversions[[to_units]]
    total_scale <- scalar_to / scalar_from
    
    output <- .data |>
      dplyr::mutate(
        x = x * total_scale,
        y = y * total_scale,
        z = z * total_scale
      )
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

  if (!inherits(output, 'motion_trace')) {
    class(output) <- c('motion_trace', class(output))
  }
  
  attr(output, 'metadata')$units <- to_units
  attr(output, 'metadata')$origin_type <- if (isTRUE(norm)) 'starting_pos' else if (is.numeric(norm)) 'custom' else 'none'
  
  return(output)
}
