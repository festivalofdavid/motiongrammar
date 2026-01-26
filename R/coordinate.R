# segment name: coordinate ---

#' Convert latitude and longitude to xy (and z if we have it from altitude) metrics
#' 
#' @param from Character: usually 'latlong'
#' @param to Character: usually 'xyz'
#' @param crs Number: GPS system, defaults to most common
#' @return An object of class \code{motion_trace}.
#' @export
coordinate <- function(.data, 
                      from = 'latlong', 
                      to = 'xyz', 
                      crs = 3857){
  
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
    
    return(output)
  }
  
  stop('Transformation not supported')
}

test <- initiate(verbose = FALSE) |> 
  coordinate()