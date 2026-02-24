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

    valid <- !is.na(.data$lat) & !is.na(.data$lng)

    output <- .data
    output$x <- NA_real_
    output$y <- NA_real_
    output$z <- if('altitude' %in% names(.data)) .data$altitude else 0

    if(any(valid)){
      coords <- .data[valid, ] |>
        sf::st_as_sf(coords = c('lng', 'lat'), crs = 4326, remove = FALSE) |>
        sf::st_transform(crs = crs) |>
        sf::st_coordinates()

      output$x[valid] <- coords[,1]
      output$y[valid] <- coords[,2]
    }

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
    first_valid <- which(!is.na(output$x) & !is.na(output$y))[1]
    if(is.na(first_valid)) return(output)
    origin_x <- output$x[first_valid]
    origin_y <- output$y[first_valid]
    origin_z <- output$z[first_valid]
  } else if(is.numeric(norm) && length(norm) >= 2){
    origin_x <- norm[1]
    origin_y <- norm[2]
    origin_z <- if(length(norm) == 3) norm[3] else 0
  } else {
    stop("coordinate(): 'norm' must be TRUE/FALSE or a numeric vector of length >= 2.")
  }

  attr(output, 'norm_origin') <- c(origin_x, origin_y, origin_z)

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

# segment name: coord_drop_zero_gps ---

#' Drop lat/lng rows where both are zero (no GPS signal)
#' @param output Data frame with lat and lng columns.
#' @return Modified data frame with zero-signal lat/lng set to NA.
#' @keywords internal
coord_drop_zero_gps <- function(output){
  if(!all(c('lat', 'lng') %in% names(output))) return(output)

  is_zero <- output$lat == 0 & output$lng == 0
  is_zero[is.na(is_zero)] <- FALSE
  zero_rows <- which(is_zero)

  if(length(zero_rows) > 0){
    output$lat[is_zero] <- NA_real_
    output$lng[is_zero] <- NA_real_
  }

  attr(output, 'dropped_zero_gps') <- list(
    n = length(zero_rows),
    pct = length(zero_rows) / nrow(output) * 100,
    rows = zero_rows
  )

  output
}

# segment name: coord_drop_outliers ---

#' Detect and replace outlier coordinates with NA
#' @param output Data frame with x, y columns.
#' @param k Numeric; IQR multiplier for outlier fences.
#' @return Modified data frame with outlier x/y/z set to NA.
#' @keywords internal
coord_drop_outliers <- function(output, k = 3){
  detect_bounds <- function(vals, k){
    q <- stats::quantile(vals, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    c(lower = q[1] - k * iqr, upper = q[2] + k * iqr)
  }

  x_bounds <- detect_bounds(output$x, k)
  y_bounds <- detect_bounds(output$y, k)

  is_outlier <- (output$x < x_bounds['lower'] | output$x > x_bounds['upper']) |
                (output$y < y_bounds['lower'] | output$y > y_bounds['upper'])
  is_outlier[is.na(is_outlier)] <- FALSE

  outlier_rows <- which(is_outlier)

  if(length(outlier_rows) > 0){
    output$x[is_outlier] <- NA_real_
    output$y[is_outlier] <- NA_real_
    if('z' %in% names(output)) output$z[is_outlier] <- NA_real_
  }

  attr(output, 'dropped_outliers') <- list(
    n = length(outlier_rows),
    pct = length(outlier_rows) / nrow(output) * 100,
    rows = outlier_rows,
    x_bounds = x_bounds,
    y_bounds = y_bounds
  )

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
#' @param drop_outliers Logical; whether outliers were dropped.
#' @param rows_before Integer; row count of the input before any transformation.
#' @param cols_before Character vector; column names of the input before transformation.
#' @return Data frame with quality attribute set.
#' @keywords internal
coord_quality_log <- function(output, from, to, crs, from_units, to_units, norm, rotate, drop_outliers = FALSE,
                              rows_before = NULL, cols_before = NULL){
  qual <- attr(output, 'quality')
  if(is.null(qual)) qual <- list()

  # --- Outlier / invalid coordinate detection ---
  outliers <- list()

  dropped_zero <- attr(output, 'dropped_zero_gps')
  if(!is.null(dropped_zero) && dropped_zero$n > 0){
    outliers$zero_gps <- list(
      description = sprintf("%d rows with Lat/Lng both zero (no signal) set to NA", dropped_zero$n),
      n = dropped_zero$n,
      pct = dropped_zero$pct,
      rows = dropped_zero$rows
    )
  }

  detect_iqr_outliers <- function(vals, label, k = 3){
    vals_clean <- vals[!is.na(vals)]
    if(length(vals_clean) < 2) return(NULL)
    q <- stats::quantile(vals_clean, c(0.25, 0.75))
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

  if(drop_outliers){
    dropped <- attr(output, 'dropped_outliers')
    if(!is.null(dropped) && dropped$n > 0){
      outliers$dropped <- list(
        description = sprintf("%d outlier rows set to NA (IQR fences: x [%.2f, %.2f], y [%.2f, %.2f])",
                              dropped$n,
                              dropped$x_bounds['lower'], dropped$x_bounds['upper'],
                              dropped$y_bounds['lower'], dropped$y_bounds['upper']),
        n = dropped$n,
        pct = dropped$pct,
        rows = dropped$rows,
        x_bounds = dropped$x_bounds,
        y_bounds = dropped$y_bounds
      )
    }
  }

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

  entry <- list(
    step            = "coordinate",
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),

    # Row / column provenance
    rows_before = if (!is.null(rows_before)) rows_before else nrow(output),
    rows_after  = nrow(output),
    cols_before = if (!is.null(cols_before)) cols_before else character(0),
    cols_after  = names(output),

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
    origin_values = if(isTRUE(norm)) attr(output, 'norm_origin')[1:2]
                    else if(is.numeric(norm)) norm else NA,

    # Rotation
    rotated = !is.null(rotate),
    rotation_angle = if(!is.null(rotate)) theta else NA,
    rotation_degrees = if(!is.null(rotate)) theta * (180 / pi) else NA,
    rotation_sideline = if(!is.null(rotate)) rotate else NULL,

    # Outlier detection parameters
    outlier_iqr_k = 3,

    # Post-transform summary
    x_range = if(all(is.na(output$x))) NA_real_ else diff(range(output$x, na.rm = TRUE)),
    y_range = if(all(is.na(output$y))) NA_real_ else diff(range(output$y, na.rm = TRUE)),
    z_present = ('z' %in% names(output) && any(output$z != 0, na.rm = TRUE)),
    total_rows = nrow(output),

    # Outliers
    outliers = outliers,
    n_outlier_rows = length(unique(unlist(lapply(outliers, function(o) o$rows)))),
    outlier_types = names(outliers),

    # Package dependencies
    dependencies = setNames(dep_versions, coord_deps)
  )

  qual$coordinate <- append(qual$coordinate, list(entry))
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
#' @param drop_outliers Logical; if TRUE, replaces coordinates that fall outside 3 * IQR fences with NA.
#' @return An object of class \code{motion_trace}.
#' @export
coordinate <- function(.data,
                      from = 'latlong',
                      to = 'xyz',
                      crs = 3857,
                      norm = FALSE,
                      from_units = NULL,
                      to_units = 'm',
                      rotate = NULL,
                      drop_outliers = FALSE){

  validate_motion_trace(.data, 'coordinate')
  rows_before <- nrow(.data)
  cols_before <- names(.data)
  conversions <- coord_conversions(from_units, to_units)

  output <- coord_drop_zero_gps(.data)

  output <- coord_project(output, from, to, crs, from_units, to_units, conversions)

  if(!inherits(output, 'motion_trace')){
    class(output) <- c('motion_trace', class(output))
  }

  output <- coord_normalise(output, norm)

  output <- coord_rotate(output, rotate, crs)

  if(!inherits(output, 'motion_trace')){
    class(output) <- c('motion_trace', class(output))
  }

  if(drop_outliers){
    output <- coord_drop_outliers(output)
  }

  attr(output, 'metadata')$units <- to_units
  attr(output, 'metadata')$origin_type <- if(isTRUE(norm)) 'starting_pos' else if(is.numeric(norm)) 'custom' else 'none'

  output <- coord_quality_log(output, from, to, crs, from_units, to_units, norm, rotate, drop_outliers,
                              rows_before = rows_before, cols_before = cols_before)

  return(output)
}
