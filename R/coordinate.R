# segment name: coord_conversions ---

#' Build and validate unit conversion factors
#' @param from_units Character or NULL; source units.
#' @param to_units Character; target units.
#' @return Named list of conversion factors.
#' @keywords internal
coord_conversions <- function(from_units = NULL, to_units = 'm'){
  conversions <- list(
    # Metric
    'mm' = 1000,
    'cm' = 100,
    'm' = 1,
    'km' = 0.001,
    # Imperial
    'in' = 39.3701,
    'ft' = 3.28084,
    'yd' = 1.09361,
    'mi' = 0.000621371)

  if(!(to_units %in% names(conversions))){
    stop('Unsupported to_units. Must be one of: ',
         paste(names(conversions), collapse = ', '))
  }

  if(!is.null(from_units) && !(from_units %in% names(conversions))){
    stop('from_units must be one of: ', paste(names(conversions), collapse = ', '))
  }

  conversions
}

# segment name: coord_project ---

#' Project or convert coordinate data
#' @param .data A data frame with spatial columns.
#' @param from Character; source coordinate type.
#' @param to Character; target coordinate type.
#' @param crs Numeric; target CRS for latlong projection.
#' @param from_units Character or NULL; source units.
#' @param to_units Character; target units.
#' @param conversions Named list of conversion factors.
#' @return Data frame with x, y, z columns.
#' @keywords internal
coord_project <- function(.data, from, to, crs, from_units, to_units, conversions){
  if(from == 'latlong' && to == 'xyz'){
    required_cols <- c('lat', 'lng')
    if(!all(required_cols %in% names(.data))){
      stop('Input data must contain "lat" and "lng" columns.')
    }

    coords <- .data |>
      sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326, remove = FALSE) |>
      sf::st_transform(crs = crs) |>
      sf::st_coordinates()

    output <- .data |>
      dplyr::mutate(
        x = coords[,1],
        y = coords[,2],
        z = if('altitude' %in% names(.data)) altitude else 0)

  } else if(from == 'xyz' && to == 'xyz'){
    if(!all(c('x', 'y') %in% names(.data))){
      stop('Input data must contain "x" and "y" columns.')
    }

    if(!is.null(from_units)){
      scalar_from <- conversions[[from_units]]
      scalar_to <- conversions[[to_units]]
      total_scale <- scalar_to / scalar_from

      output <- .data |>
        dplyr::mutate(
          x = x * total_scale,
          y = y * total_scale,
          z = if('z' %in% names(.data)) z * total_scale else 0
        )
    } else {
      output <- .data
      if(!'z' %in% names(output)){
        output <- output |> dplyr::mutate(z = 0)
      }
    }
  } else {
    stop('Transformation path not supported. Use "latlong" to "xyz" or "xyz" to "xyz".')
  }

  output
}

# segment name: coord_normalise ---

#' Normalise coordinates to an origin
#' @param output Data frame with x, y, z columns.
#' @param norm Logical or numeric; normalisation anchor.
#' @return Modified data frame.
#' @keywords internal
coord_normalise <- function(output, norm){
  if(isFALSE(norm)) return(output)

  if(isTRUE(norm)){
    origin_x <- output$x[1]
    origin_y <- output$y[1]
    origin_z <- output$z[1]
  } else if(is.numeric(norm) && length(norm) >= 2){
    origin_x <- norm[1]
    origin_y <- norm[2]
    origin_z <- if(length(norm) == 3) norm[3] else 0
  }

  output |>
    dplyr::mutate(
      x = x - origin_x,
      y = y - origin_y,
      z = z - origin_z
    )
}

# segment name: coord_rotate ---

#' Rotate coordinates so a sideline aligns with the x-axis
#' @param output Data frame with x, y columns.
#' @param rotate NULL or a list of two c(lat, lng) vectors.
#' @param crs Numeric; CRS for projecting sideline points.
#' @return Modified data frame (with rotation_angle in metadata).
#' @keywords internal
coord_rotate <- function(output, rotate, crs){
  if(is.null(rotate)) return(output)

  if(!is.list(rotate) || length(rotate) != 2){
    stop('rotate must be a list of two c(lat, lng) vectors defining a sideline.')
  }
  sideline <- rotate

  sideline_sf <- sf::st_as_sf(
    data.frame(lng = c(sideline[[1]][2], sideline[[2]][2]),
               lat = c(sideline[[1]][1], sideline[[2]][1])),
    coords = c('lng', 'lat'), crs = 4326
  ) |> sf::st_transform(crs = crs) |> sf::st_coordinates()

  dx <- sideline_sf[2, 1] - sideline_sf[1, 1]
  dy <- sideline_sf[2, 2] - sideline_sf[1, 2]
  theta <- atan2(dy, dx)

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
  output
}

# segment name: coord_quality ---

#' Build and attach the coordinate quality log
#' @param output Data frame with x, y, z columns.
#' @param from Character; source coordinate type.
#' @param to Character; target coordinate type.
#' @param crs Numeric; CRS used for projection.
#' @param from_units Character or NULL; source units.
#' @param to_units Character; target units.
#' @param norm Logical or numeric; normalisation setting.
#' @param rotate NULL or list; rotation setting.
#' @return Data frame with quality attribute set.
#' @keywords internal
coord_quality_log <- function(output, from, to, crs, from_units, to_units, norm, rotate){
  qual <- attr(output, 'quality')
  if(is.null(qual)) qual <- list()

  # --- Outlier / invalid coordinate detection ---
  outliers <- list()

  if(all(c('lat', 'lng') %in% names(output))){
    zero_gps <- which(output$lat == 0 & output$lng == 0)
    if(length(zero_gps) > 0){
      outliers$zero_gps <- list(
        description = "Lat/Lng both zero (no signal)",
        n = length(zero_gps),
        pct = length(zero_gps) / nrow(output) * 100,
        rows = zero_gps
      )
    }
  }

  detect_iqr_outliers <- function(vals, label, k = 3){
    q <- stats::quantile(vals, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lower <- q[1] - k * iqr
    upper <- q[2] + k * iqr
    idx <- which(vals < lower | vals > upper)
    if(length(idx) > 0){
      list(
        description = sprintf("%s outside %.1f * IQR (%.2f to %.2f)", label, k, lower, upper),
        n = length(idx),
        pct = length(idx) / length(vals) * 100,
        rows = idx,
        bounds = c(lower = lower, upper = upper)
      )
    }
  }

  x_out <- detect_iqr_outliers(output$x, "X", k = 3)
  if(!is.null(x_out)) outliers$x_iqr <- x_out

  y_out <- detect_iqr_outliers(output$y, "Y", k = 3)
  if(!is.null(y_out)) outliers$y_iqr <- y_out

  # --- Resolve CRS description ---
  crs_description <- NA_character_
  if(from == 'latlong'){
    crs_description <- tryCatch({
      crs_obj <- sf::st_crs(crs)
      if(!is.na(crs_obj$Name)) crs_obj$Name else paste("EPSG:", crs)
    }, error = function(e) paste("EPSG:", crs))
  }

  # --- Capture package dependencies ---
  coord_deps <- c('sf', 'dplyr')
  if(!is.null(rotate)) coord_deps <- c(coord_deps, 'sf')
  coord_deps <- unique(coord_deps)

  dep_versions <- vapply(coord_deps, function(pkg){
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "unknown")
  }, character(1))

  # Retrieve rotation angle from metadata if rotation was applied
  theta <- attr(output, 'metadata')$rotation_angle

  qual$coordinate <- list(
    step = "coordinate",
    timestamp = Sys.time(),

    # Conversion path
    from = from,
    to = to,
    latlong_to_xyz = (from == 'latlong' && to == 'xyz'),

    # CRS detail
    crs_code = if(from == 'latlong') crs else NA,
    crs_source = if(from == 'latlong') 4326 else NA,
    crs_target = if(from == 'latlong') crs else NA,
    crs_description = crs_description,

    # Unit conversion
    units_converted = !is.null(from_units),
    from_units = if(!is.null(from_units)) from_units else NA,
    to_units = to_units,

    # Normalisation
    normalised = !isFALSE(norm),
    origin_type = if(isTRUE(norm)) 'starting_pos' else if(is.numeric(norm)) 'custom' else 'none',
    origin_values = if(isTRUE(norm)) c(output$x[1] + (output$x[1] - output$x[1]),
                                         output$y[1] + (output$y[1] - output$y[1]))
                    else if(is.numeric(norm)) norm else NA,

    # Rotation
    rotated = !is.null(rotate),
    rotation_angle = if(!is.null(rotate)) theta else NA,
    rotation_degrees = if(!is.null(rotate)) theta * (180 / pi) else NA,

    # Post-transform summary
    x_range = diff(range(output$x, na.rm = TRUE)),
    y_range = diff(range(output$y, na.rm = TRUE)),
    z_present = ('z' %in% names(output) && any(output$z != 0, na.rm = TRUE)),
    total_rows = nrow(output),

    # Outliers
    outliers = outliers,
    n_outlier_rows = length(unique(unlist(lapply(outliers, function(o) o$rows)))),
    outlier_types = names(outliers),

    # Package dependencies
    dependencies = setNames(dep_versions, coord_deps)
  )

  attr(output, 'quality') <- qual
  output
}

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

  conversions <- coord_conversions(from_units, to_units)

  output <- coord_project(.data, from, to, crs, from_units, to_units, conversions)

  if(!inherits(output, 'motion_trace')){
    class(output) <- c('motion_trace', class(output))
  }

  output <- coord_normalise(output, norm)

  output <- coord_rotate(output, rotate, crs)

  if(!inherits(output, 'motion_trace')){
    class(output) <- c('motion_trace', class(output))
  }

  attr(output, 'metadata')$units <- to_units
  attr(output, 'metadata')$origin_type <- if(isTRUE(norm)) 'starting_pos' else if(is.numeric(norm)) 'custom' else 'none'

  output <- coord_quality_log(output, from, to, crs, from_units, to_units, norm, rotate)

  return(output)
}
