# segment name: auth ---

#' Refresh Strava API Tokens
#'
#' @param tokens A list containing client_id, client_secret, refresh_token, and access_token.
#' @param verbose Logical; if TRUE, prints status messages.
#' @return A list of updated tokens.
#' @keywords internal
get_valid_tokens <- function(tokens,
                            verbose = TRUE){
  now <- as.integer(Sys.time())

  if(!is.null(tokens$expires_at) && (tokens$expires_at - now) > 120){
    return(tokens)
  }

  if(verbose){
    message('Refreshing Strava tokens...')
  }

  resp <- httr2::request('https://www.strava.com/oauth/token') |>
    httr2::req_body_form(client_id = tokens$client_id,
      client_secret = tokens$client_secret,
      grant_type = 'refresh_token',
      refresh_token = tokens$refresh_token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tokens$access_token <- resp$access_token
  tokens$refresh_token <- resp$refresh_token
  tokens$expires_at <- resp$expires_at

  path <- path.expand('~/strava_tokens.json')
  writeLines(jsonlite::toJSON(tokens,
                              auto_unbox = TRUE,
                              pretty = TRUE),
    path
  )

  return(tokens)
}

# segment name: streams ---

#' Fetch Physics Streams from Strava
#'
#' @param activity_id Character; the Strava activity ID.
#' @param access_token Character; valid OAuth2 access token.
#' @param start_time_iso Character; ISO 8601 start date.
#' @keywords internal
get_physics_streams <- function(activity_id,
                               access_token,
                               start_time_iso,
                               template = NULL){

  url <- paste0('https://www.strava.com/api/v3/activities/',
                activity_id,
                '/streams')

  t <- if (!is.null(template)) template else list(
    coord_system = "gps",
    stream_time = "time",
    stream_latlng = "latlng",
    stream_lat = NULL,
    stream_lng = NULL,
    stream_altitude = "altitude",
    stream_x = NULL,
    stream_y = NULL,
    stream_z = NULL,
    stream_velocity = NULL,
    stream_acceleration = NULL,
    stream_extra = NULL
  )

  keys_vec <- unique(c(
    t$stream_time,
    t$stream_latlng, t$stream_lat, t$stream_lng,
    t$stream_altitude,
    t$stream_x, t$stream_y, t$stream_z,
    t$stream_velocity, t$stream_acceleration,
    if (!is.null(t$stream_extra)) unname(t$stream_extra) else NULL
  ))
  keys_vec <- keys_vec[!is.na(keys_vec)]

  resp <- httr2::request(url) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_url_query(keys = paste(keys_vec, collapse = ","),
                         key_by_type = 'true') |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  start_unix <- as.numeric(lubridate::as_datetime(start_time_iso))
  n_rows <- if (length(resp) > 0) length(resp[[1]]$data) else 0L
  output <- list()

  for (key in names(resp)) {
    raw <- resp[[key]]$data
    if (key == t$stream_time) {
      output[["unix_time"]] <- start_unix + unlist(raw)
    } else if (!is.null(t$stream_latlng) && key == t$stream_latlng) {
      output[["lat"]] <- purrr::map_dbl(raw, 1)
      output[["lng"]] <- purrr::map_dbl(raw, 2)
    } else if (!is.null(t$stream_lat) && key == t$stream_lat) {
      output[["lat"]] <- unlist(raw)
    } else if (!is.null(t$stream_lng) && key == t$stream_lng) {
      output[["lng"]] <- unlist(raw)
    } else if (!is.null(t$stream_altitude) && key == t$stream_altitude) {
      output[["altitude"]] <- unlist(raw)
    } else if (!is.null(t$stream_x) && key == t$stream_x) {
      output[["x"]] <- unlist(raw)
    } else if (!is.null(t$stream_y) && key == t$stream_y) {
      output[["y"]] <- unlist(raw)
    } else if (!is.null(t$stream_z) && key == t$stream_z) {
      output[["z"]] <- unlist(raw)
    } else if (!is.null(t$stream_velocity) && key == t$stream_velocity) {
      output[["velocity"]] <- unlist(raw)
    } else if (!is.null(t$stream_acceleration) && key == t$stream_acceleration) {
      output[["acceleration"]] <- unlist(raw)
    } else if (!is.null(t$stream_extra) && key %in% unname(t$stream_extra)) {
      extra_nms <- names(t$stream_extra)
      idx <- which(unname(t$stream_extra) == key)[1]
      out_nm <- if (!is.null(extra_nms) && nchar(extra_nms[idx]) > 0) extra_nms[idx] else key
      output[[out_nm]] <- unlist(raw)
    }
  }

  coord_sys <- match.arg(t$coord_system %||% "gps", c("gps", "local"))
  core_cols <- if (coord_sys == "gps") c("unix_time", "lat", "lng", "altitude")
               else c("unix_time", "x", "y", "z")

  for (col in core_cols) {
    if (is.null(output[[col]])) output[[col]] <- rep(NA_real_, n_rows)
  }

  extra_cols <- setdiff(names(output), core_cols)
  tibble::as_tibble(output[c(core_cols, extra_cols)])
}

# segment name: initiate_logic ---

#' Auto-Detect and Parse Messy CSV Files
#' @keywords internal
initiate_guess_csv <- function(.data_path,
                               source = "gps",
                               confidence_threshold = 0.8){

  source <- match.arg(source, choices = c("gps", "local", "auto"))

  all_lines <- readr::read_lines(.data_path)
  comma_counts <- stringr::str_count(all_lines, ',')

  line_stats <- tibble::tibble(count = comma_counts) |>
    dplyr::filter(count > 0) |>
    dplyr::group_by(count) |>
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') |>
    dplyr::arrange(dplyr::desc(n))

  if(nrow(line_stats) == 0){
    stop('No valid CSV structure detected in file.')
  }

  target_commas <- line_stats$count[1]
  first_data_idx <- which(comma_counts == target_commas)[1]

  if(is.na(first_data_idx)){
    stop('Could not identify data block in file.')
  }

  if(first_data_idx >= 2){
    # If the first line of the data block contains letters it IS the header
    # (standard device-export layout: preamble rows → header → data).
    # Only fall back to the backward preamble search for the rare case where
    # data rows have more commas than the header (e.g. trailing empty fields).
    if(stringr::str_detect(all_lines[first_data_idx], '^[A-Za-z_]')){
      header_idx <- first_data_idx
    } else {
      potential_headers <- all_lines[1:(first_data_idx - 1)]
      header_candidates <- which(stringr::str_detect(potential_headers, '[A-Za-z]'))

      if(length(header_candidates) == 0){
        stop('Could not identify a valid header row.')
      }

      header_idx <- utils::tail(header_candidates, 1)
    }
  } else {
    # Standard CSV: header is line 1, same comma count as data rows
    if(!stringr::str_detect(all_lines[1], '[A-Za-z]')){
      stop('Could not identify a valid header row.')
    }
    header_idx <- 1L
  }

  raw_header <- all_lines[header_idx]
  raw_cols <- raw_header |>
    stringr::str_split(',') |>
    purrr::pluck(1) |>
    stringr::str_trim() |>
    stringr::str_remove_all('^"|"$')   # strip surrounding quotes from write.csv output

  clean_names <- raw_cols[raw_cols != '']

  if(length(clean_names) == 0){
    stop('No valid column names found in header.')
  }

  clean_names <- make.unique(clean_names, sep = '_')
  data_lines <- all_lines[comma_counts == target_commas]

  # When the header shares the same comma count as data rows it is included in
  # data_lines; remove it so it isn't duplicated when prepended as raw_header
  if(header_idx == first_data_idx){
    data_lines <- data_lines[-1]
  }

  if(length(data_lines) == 0){
    stop('No data rows found matching the identified structure.')
  }

  df <- suppressWarnings(
    readr::read_csv(
      I(paste0(c(raw_header, data_lines), collapse = '\n')),
      show_col_types = FALSE,
      name_repair = 'minimal',
      col_types = readr::cols(.default = readr::col_character())
    )
  )

  df <- df[, 1:length(clean_names), drop = FALSE]
  colnames(df) <- clean_names

  if(source == "auto"){
    col_lower <- stringr::str_to_lower(colnames(df))

    has_lat <- any(stringr::str_detect(col_lower, 'lat'))
    has_lon <- any(stringr::str_detect(col_lower, 'lon'))
    has_x <- any(col_lower == 'x' | stringr::str_detect(col_lower, 'coord_x'))
    has_y <- any(col_lower == 'y' | stringr::str_detect(col_lower, 'coord_y'))

    if(has_lat || has_lon){
      source <- "gps"
    } else if(has_x || has_y){
      source <- "local"
    } else {
      source <- "gps"
    }

    message(sprintf("Auto-detected coordinate system: %s", source))
  }

  if(source == "gps"){
    targets <- list(
      unix_time = list(
        primary = c('unix', 'timestamp', 'unixtime', 'time_ms', 'utc', 'epoch'),
        fallback = c('time', 'datetime', 'date', 'frame', 'frameid', 'frame_id', 'frame_number')
      ),
      lat = list(
        primary = c('latitude', 'lat'),
        fallback = c('y', 'coord_y', 'lat_deg')
      ),
      lng = list(
        primary = c('longitude', 'lon', 'long', 'lng'),
        fallback = c('x', 'coord_x', 'lon_deg', 'long_deg')
      ),
      altitude = list(
        primary = c('altitude', 'elevation', 'height', 'alt', 'elev'),
        fallback = c('z', 'alt_m', 'elevation_m')
      )
    )
    output_cols <- c('unix_time', 'lat', 'lng', 'altitude')
  } else {
    targets <- list(
      unix_time = list(
        primary = c('unix', 'timestamp', 'unixtime', 'time_ms', 'utc'),
        fallback = c('time', 'datetime', 'date', 'epoch')
      ),
      x = list(
        primary = c('x', 'coord_x', 'pos_x'),
        fallback = c('longitude', 'lon', 'long')
      ),
      y = list(
        primary = c('y', 'coord_y', 'pos_y'),
        fallback = c('latitude', 'lat')
      ),
      z = list(
        primary = c('z', 'coord_z', 'pos_z', 'height'),
        fallback = c('altitude', 'elevation', 'alt')
      )
    )
    output_cols <- c('unix_time', 'x', 'y', 'z')
  }

  find_best_match <- function(target_patterns, available_cols){
    available_lower <- stringr::str_to_lower(available_cols)

    for(pattern in target_patterns){
      pattern_lower <- stringr::str_to_lower(pattern)
      exact_match <- which(available_lower == pattern_lower)
      if(length(exact_match) > 0){
        return(available_cols[exact_match[1]])
      }
    }

    best_score <- 0
    best_match <- NULL

    for(pattern in target_patterns){
      pattern_lower <- stringr::str_to_lower(pattern)

      for(i in seq_along(available_cols)){
        col_lower <- available_lower[i]
        sim <- 1 - stringdist::stringdist(pattern_lower, col_lower, method = 'jw')

        if(sim > best_score && sim >= confidence_threshold){
          best_score <- sim
          best_match <- available_cols[i]
        }
      }
    }

    return(best_match)
  }

  # Apply fuzzy mappings - with exclusion to prevent double-matching
  mappings <- list()
  available_cols <- colnames(df)

  for(target_name in names(targets)){
    match <- find_best_match(targets[[target_name]]$primary, available_cols)

    if(is.null(match) && !is.null(targets[[target_name]]$fallback)){
      match <- find_best_match(targets[[target_name]]$fallback, available_cols)
    }

    mappings[[target_name]] <- match

    # Remove matched column from pool to prevent double-matching
    if(!is.null(match)){
      available_cols <- setdiff(available_cols, match)
    }
  }

  for(target_name in names(mappings)){
    original_col <- mappings[[target_name]]

    if(!is.null(original_col) && original_col %in% colnames(df)){
      df <- df |>
        dplyr::rename(!!target_name := !!original_col)
    }
  }

  if('unix_time' %in% colnames(df)){
    df <- df |>
      dplyr::mutate(unix_time = convert_to_unix(unix_time))
  }

  other_cols <- setdiff(output_cols, 'unix_time')
  for(col in other_cols){
    if(col %in% colnames(df)){
      df <- df |>
        dplyr::mutate(!!col := suppressWarnings(as.numeric(.data[[col]])))
    }
  }

  output <- df |>
    dplyr::select(dplyr::any_of(output_cols))

  missing_cols <- setdiff(output_cols, colnames(output))
  for(col in missing_cols){
    output[[col]] <- NA_real_
  }

  output <- output |>
    dplyr::select(dplyr::all_of(output_cols))

  if(nrow(output) == 0){
    stop('No valid data rows after parsing.')
  }

  all_na <- all(sapply(output, function(x) all(is.na(x))))
  if(all_na){
    warning('All columns are NA - column matching may have failed. Check your column names.')
  }

  # Store column mapping as attribute for later retrieval
  attr(output, 'column_mapping') <- mappings
  attr(output, 'skip') <- as.integer(header_idx - 1L)
  attr(output, 'coord_system_detected') <- source

  return(output)
}

#' Manually Parse CSV Files
#' @keywords internal
initiate_manual_csv <- function(.data_path,
                               skip = 0,
                               col_unix,
                               col_lat,
                               col_lng,
                               col_altitude = NULL,
                               coord_system = c("gps", "local"),
                               max_empty_lines = 3,
                               n_max = NULL,
                               comment = "#"){

  coord_system <- match.arg(coord_system)

  if(!file.exists(.data_path)){
    stop(sprintf("File not found: %s", .data_path))
  }

  if(missing(col_unix)) stop("col_unix must be specified")
  if(missing(col_lat)) stop("col_lat must be specified")
  if(missing(col_lng)) stop("col_lng must be specified")

  all_lines <- readr::read_lines(.data_path)

  if(skip > 0){
    all_lines <- all_lines[(skip + 1):length(all_lines)]
  }

  all_lines <- all_lines[!stringr::str_detect(all_lines, sprintf("^\\s*%s", comment))]

  empty_pattern <- stringr::str_detect(all_lines, "^\\s*$")

  consecutive_empty <- 0
  stop_at <- length(all_lines)

  for(i in seq_along(all_lines)){
    if(empty_pattern[i]){
      consecutive_empty <- consecutive_empty + 1
      if(consecutive_empty >= max_empty_lines){
        stop_at <- i - max_empty_lines
        break
      }
    } else {
      consecutive_empty <- 0
    }
  }

  data_to_read <- all_lines[1:stop_at]
  data_to_read <- data_to_read[!empty_pattern[1:stop_at]]

  df <- suppressWarnings(
    readr::read_csv(
      I(paste(data_to_read, collapse = "\n")),
      n_max = n_max,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
  )

  if(nrow(df) == 0){
    stop("No data rows found after parsing")
  }

  col_names_lower <- stringr::str_to_lower(colnames(df))

  unix_idx <- which(col_names_lower == stringr::str_to_lower(col_unix))
  lat_idx <- which(col_names_lower == stringr::str_to_lower(col_lat))
  lng_idx <- which(col_names_lower == stringr::str_to_lower(col_lng))

  if(length(unix_idx) == 0){
    stop(sprintf("Column '%s' not found. Available: %s",
                 col_unix, paste(colnames(df), collapse = ", ")))
  }

  if(length(lat_idx) == 0){
    stop(sprintf("Column '%s' not found. Available: %s",
                 col_lat, paste(colnames(df), collapse = ", ")))
  }

  if(length(lng_idx) == 0){
    stop(sprintf("Column '%s' not found. Available: %s",
                 col_lng, paste(colnames(df), collapse = ", ")))
  }

  alt_idx <- NULL
  if(!is.null(col_altitude)){
    alt_idx <- which(col_names_lower == stringr::str_to_lower(col_altitude))
    if(length(alt_idx) == 0){
      warning(sprintf("Column '%s' not found. Setting altitude to NA.", col_altitude))
    }
  }

  if(coord_system == "gps"){
    output_names <- c('unix_time', 'lat', 'lng', 'altitude')
  } else {
    output_names <- c('unix_time', 'x', 'y', 'z')
  }

  output <- tibble::tibble(
    unix_time = df[[unix_idx[1]]],
    coord1 = df[[lat_idx[1]]],
    coord2 = df[[lng_idx[1]]]
  )

  if(!is.null(alt_idx) && length(alt_idx) > 0){
    output$coord3 <- df[[alt_idx[1]]]
  } else {
    output$coord3 <- NA_character_
  }

  colnames(output) <- output_names

  output <- output |>
    dplyr::mutate(unix_time = convert_to_unix(unix_time))

  output <- output |>
    dplyr::mutate(
      !!output_names[2] := suppressWarnings(as.numeric(.data[[output_names[2]]])),
      !!output_names[3] := suppressWarnings(as.numeric(.data[[output_names[3]]])),
      !!output_names[4] := suppressWarnings(as.numeric(.data[[output_names[4]]]))
    )

  if(nrow(output) == 0){
    stop('No valid data rows after parsing')
  }

  na_unix <- sum(is.na(output$unix_time))
  na_coord1 <- sum(is.na(output[[output_names[2]]]))
  na_coord2 <- sum(is.na(output[[output_names[3]]]))

  if(na_unix > 0){
    warning(sprintf("%d unix_time values are NA", na_unix))
  }
  if(na_coord1 > 0){
    warning(sprintf("%d %s values are NA", na_coord1, output_names[2]))
  }
  if(na_coord2 > 0){
    warning(sprintf("%d %s values are NA", na_coord2, output_names[3]))
  }

  # Store column mapping as attribute
  if(coord_system == "gps"){
    attr(output, 'column_mapping') <- list(
      unix_time = col_unix,
      lat = col_lat,
      lng = col_lng,
      altitude = col_altitude %||% "[not specified]"
    )
  } else {
    attr(output, 'column_mapping') <- list(
      unix_time = col_unix,
      x = col_lat,
      y = col_lng,
      z = col_altitude %||% "[not specified]"
    )
  }

  return(output)
}

#' Parse GPX Files
#' @keywords internal
initiate_gpx <- function(.data_path){

  if(!requireNamespace("sf", quietly = TRUE)){
    stop("sf package required for GPX files. Install with: install.packages('sf')")
  }

  gpx_data <- sf::st_read(.data_path, layer = "track_points", quiet = TRUE)

  coords <- sf::st_coordinates(gpx_data)

  trace <- tibble::tibble(
    unix_time = as.numeric(gpx_data$time),
    lat = coords[, 2],
    lng = coords[, 1],
    altitude = dplyr::coalesce(gpx_data$ele, NA_real_)
  )

  attr(trace, 'column_mapping') <- list(
    unix_time = "time",
    lat = "latitude",
    lng = "longitude",
    altitude = "ele"
  )

  return(trace)
}

# segment name: helpers ---

#' Null Coalescing Operator
#' @keywords internal
`%||%` <- function(a, b){
  if(is.null(a)) b else a
}

#' Validate a motion_trace Object
#'
#' Called at the entry of every major pipeline function. Checks that the input
#' is a \code{motion_trace}, has \code{unix_time}, and optionally that specific
#' columns are present.
#'
#' @param .data Object to validate.
#' @param call Character; calling function name, used in error messages.
#' @param requires Character vector of additional required column names.
#' @keywords internal
validate_motion_trace <- function(.data, call = 'function', requires = character(0)) {

  if (!inherits(.data, 'motion_trace')) {
    stop(
      call, '(): input must be a motion_trace object created by initiate(). ',
      'If a dplyr verb stripped the class, wrap the operation in elaborate() ',
      'or ensure dplyr >= 1.1 and vctrs >= 0.6 are installed.'
    )
  }

  required <- c('unix_time', requires)
  missing <- setdiff(required, names(.data))
  if (length(missing) > 0) {
    stop(
      call, '(): required column(s) missing: ',
      paste(missing, collapse = ', '),
      '. Ensure all prior pipeline steps have been completed.'
    )
  }

  if (is.null(attr(.data, 'metadata'))) {
    warning(
      call, '(): metadata attribute is missing. ',
      'Quality and reproducibility tracking will be incomplete.'
    )
  }

  invisible(.data)
}

#' vctrs restore method for motion_trace
#'
#' Ensures that dplyr verbs (filter, slice, arrange, rename, etc.) preserve
#' the motion_trace class and its metadata and quality attributes.
#'
#' @param x The result of a dplyr operation.
#' @param to The original motion_trace object.
#' @keywords internal
#' @importFrom vctrs vec_restore
#' @export
vec_restore.motion_trace <- function(x, to, ...) {
  attr(x, 'metadata') <- attr(to, 'metadata')
  attr(x, 'quality') <- attr(to, 'quality')
  class(x) <- class(to)
  x
}

#' Create Metadata
#' @keywords internal
create_metadata <- function(session,
                           source,
                           trace,
                           device_info = NULL,
                           coord_system = 'gps'){

  session_name <- if(file.exists(session)){
    basename(session)
  } else {
    as.character(session)
  }

  date_from_name <- tryCatch({
    date_patterns <- c(
      "\\d{4}-\\d{2}-\\d{2}",
      "\\d{8}",
      "\\d{2}-\\d{2}-\\d{4}"
    )

    for(pattern in date_patterns){
      match <- stringr::str_extract(session_name, pattern)
      if(!is.na(match)){
        parsed <- lubridate::parse_date_time(match, orders = c('ymd', 'dmy'))
        if(!is.na(parsed)) return(as.Date(parsed))
      }
    }
    NA
  }, error = function(e) NA)

  time_diffs <- diff(trace$unix_time)
  time_diffs <- time_diffs[!is.na(time_diffs) & time_diffs > 0]

  if(length(time_diffs) > 0){
    median_dt <- median(time_diffs)
    native_hz <- round(1 / median_dt, 2)
  } else {
    median_dt <- NA
    native_hz <- NA
  }

  time_range <- range(trace$unix_time, na.rm = TRUE)
  duration_sec <- if(!any(is.na(time_range))) diff(time_range) else NA

  pkg_version <- .mg_pkg_version()

  metadata <- list(
    name = session_name,
    source = source,
    session_id = NA_character_,

    player_id = NA_character_,
    player_name = NA_character_,
    team = NA_character_,

    device_type = if(!is.null(device_info$type)) device_info$type else NA_character_,
    device_id = if(!is.null(device_info$id)) device_info$id else NA_character_,
    device_manufacturer = if(!is.null(device_info$manufacturer)) device_info$manufacturer else NA_character_,
    firmware_version = if(!is.null(device_info$firmware)) device_info$firmware else NA_character_,

    sport = NA_character_,
    session_type = NA_character_,
    session_start = date_from_name,
    session_duration_sec = as.numeric(duration_sec),

    coordinate_system = coord_system,
    native_hz = native_hz,
    median_dt = median_dt,

    column_mapping = NULL,

    created_timestamp = Sys.time(),
    created_by = 'motiongrammar',
    package_version = pkg_version
  )

  return(metadata)
}

#' Update Metadata
#' @export
set_metadata <- function(.data, ...){

  if(!inherits(.data, 'motion_trace')){
    stop("Input must be a motion_trace object")
  }

  meta <- attr(.data, 'metadata')

  if(is.null(meta)){
    warning("No metadata found. Creating new metadata structure.")
    meta <- list()
  }

  new_meta <- list(...)

  for(key in names(new_meta)){
    meta[[key]] <- new_meta[[key]]
  }

  attr(.data, 'metadata') <- meta
  return(.data)
}

#' Normalise Unix Timestamps
#' @keywords internal
norm_unix <- function(unix_vec){

  valid_times <- unix_vec[!is.na(unix_vec)]

  if(length(valid_times) == 0){
    return(unix_vec)
  }

  median_time <- median(valid_times, na.rm = TRUE)
  digits <- floor(log10(abs(median_time))) + 1

  if(digits == 13){
    return(unix_vec / 1000)
  } else if(digits == 11 || digits == 12){
    return(unix_vec / 100)
  } else if(digits == 9 || digits == 10){
    return(unix_vec)
  } else {
    warning(sprintf("Unexpected unix_time format (%d digits). Expected 9-13 digits. Returning unchanged.", digits))
    return(unix_vec)
  }
}

#' Convert to Unix Time
#' @keywords internal
convert_to_unix <- function(time_vec){

  valid_times <- time_vec[!is.na(time_vec)]

  if(length(valid_times) == 0){
    return(as.numeric(time_vec))
  }

  numeric_vec <- suppressWarnings(as.numeric(time_vec))

  if(sum(!is.na(numeric_vec)) > length(valid_times) * 0.8){
    valid_numeric <- numeric_vec[!is.na(numeric_vec)]
    median_val <- median(valid_numeric, na.rm = TRUE)

    if(median_val > 100000){
      return(norm_unix(numeric_vec))
    } else {
      return(numeric_vec)
    }
  }

  tryCatch({
    parsed <- lubridate::parse_date_time(time_vec,
                                          orders = c("ymd HMS", "dmy HMS", "mdy HMS",
                                                    "ymd HM", "dmy HM", "mdy HM",
                                                    "ymd", "dmy", "mdy"))
    unix_time <- as.numeric(parsed)

    if(sum(!is.na(unix_time)) > length(valid_times) * 0.5){
      return(unix_time)
    } else {
      warning("Could not parse time column as datetime. Returning as numeric.")
      return(as.numeric(time_vec))
    }
  }, error = function(e){
    warning(sprintf("Time conversion failed: %s. Using numeric conversion.", e$message))
    return(as.numeric(time_vec))
  })
}

# segment name: quality_log ---

#' Initialise Quality Log
#' @keywords internal
init_quality_log <- function(trace, source){

  time_diffs <- diff(trace$unix_time)
  time_diffs <- time_diffs[!is.na(time_diffs) & time_diffs > 0]

  if(length(time_diffs) > 0){
    median_dt <- median(time_diffs)
    native_hz <- round(1 / median_dt, 2)
  } else {
    median_dt <- NA
    native_hz <- NA
  }

  if(!is.na(median_dt)){
    gap_threshold <- median_dt * 3
    gaps <- which(time_diffs > gap_threshold)
    largest_gap <- if(length(gaps) > 0) max(time_diffs[gaps]) else 0
  } else {
    gaps <- integer(0)
    largest_gap <- 0
  }

  if ('lat' %in% names(trace) && 'lng' %in% names(trace)) {
    valid_coords <- sum(!is.na(trace$lat) & !is.na(trace$lng))
  } else if ('x' %in% names(trace) && 'y' %in% names(trace)) {
    valid_coords <- sum(!is.na(trace$x) & !is.na(trace$y))
  } else {
    valid_coords <- nrow(trace)
  }
  completeness <- valid_coords / nrow(trace)

  time_range <- range(trace$unix_time, na.rm = TRUE)
  duration_sec <- diff(time_range)

  issues <- list()
  warnings <- list()

  if(completeness < 0.90){
    issues <- c(issues, sprintf("Low coordinate completeness: %.1f%%", completeness * 100))
  } else if(completeness < 0.95){
    warnings <- c(warnings, sprintf("Moderate missing coordinates: %.1f%%", (1 - completeness) * 100))
  }

  if(!is.na(native_hz)){
    if(native_hz < 1){
      issues <- c(issues, sprintf("Very low sampling rate: %.2f Hz", native_hz))
    } else if(native_hz < 5){
      warnings <- c(warnings, sprintf("Low sampling rate: %.2f Hz (recommend ≥5 Hz)", native_hz))
    }
  }

  gap_percentage <- if(length(time_diffs) > 0) length(gaps) / length(time_diffs) * 100 else 0
  if(gap_percentage > 5){
    issues <- c(issues, sprintf("High gap frequency: %d gaps (%.1f%% of intervals)",
                                length(gaps), gap_percentage))
  } else if(gap_percentage > 2){
    warnings <- c(warnings, sprintf("Moderate gap frequency: %d gaps detected", length(gaps)))
  }

  if(largest_gap > 10){
    issues <- c(issues, sprintf("Large temporal gap detected: %.1f seconds", largest_gap))
  } else if(largest_gap > 5){
    warnings <- c(warnings, sprintf("Notable gap detected: %.1f seconds", largest_gap))
  }

  if(!is.na(duration_sec)){
    if(duration_sec < 60){
      warnings <- c(warnings, sprintf("Very short session: %.0f seconds", duration_sec))
    } else if(duration_sec > 7200){
      warnings <- c(warnings, sprintf("Very long session: %.1f hours", duration_sec / 3600))
    }
  }

  if(source %in% c('strava', 'guess_csv', 'catapult_replay', 'manual_csv', 'gpx', 'template_csv') &&
     'lat' %in% names(trace) && 'lng' %in% names(trace)){
    lat_range <- diff(range(trace$lat, na.rm = TRUE))
    lng_range <- diff(range(trace$lng, na.rm = TRUE))

    if(lat_range < 0.0001 && lng_range < 0.0001){
      warnings <- c(warnings, "Very small coordinate range - indoor/stationary session?")
    }

    invalid_lat <- sum(abs(trace$lat) > 90, na.rm = TRUE)
    invalid_lng <- sum(abs(trace$lng) > 180, na.rm = TRUE)

    if(invalid_lat > 0 || invalid_lng > 0){
      issues <- c(issues, sprintf("Invalid GPS coordinates: %d latitude, %d longitude",
                                  invalid_lat, invalid_lng))
    }
  }

  n_duplicates <- sum(duplicated(trace$unix_time))
  if(n_duplicates > 0){
    if(n_duplicates > nrow(trace) * 0.01){
      issues <- c(issues, sprintf("High duplicate timestamps: %d (%.1f%%)",
                                  n_duplicates, n_duplicates / nrow(trace) * 100))
    } else {
      warnings <- c(warnings, sprintf("Duplicate timestamps: %d points", n_duplicates))
    }
  }

  qc_pass <- length(issues) == 0

  initiate_entry <- list(
    step            = "initiate",
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),
    source          = source,

    rows_before = 0L,
    rows_after  = nrow(trace),
    cols_before = character(0),
    cols_after  = names(trace),

    total_rows = nrow(trace),
    valid_coordinates = valid_coords,
    completeness = completeness,

    native_hz = native_hz,
    median_dt = median_dt,
    duration_sec = as.numeric(duration_sec),
    time_range_unix = time_range,

    n_gaps = length(gaps),
    largest_gap_sec = largest_gap,
    gap_percentage = gap_percentage,

    lat_range = if('lat' %in% names(trace)) diff(range(trace$lat, na.rm = TRUE)) else NA,
    lng_range = if('lng' %in% names(trace)) diff(range(trace$lng, na.rm = TRUE)) else NA,

    n_duplicates = n_duplicates,

    column_mapping = attr(trace, 'metadata')$column_mapping,

    qc_pass = qc_pass,
    issues = if(length(issues) > 0) unlist(issues) else character(0),
    warnings = if(length(warnings) > 0) unlist(warnings) else character(0),

    qc_thresholds = list(
      completeness_issue = 0.90,
      completeness_warning = 0.95,
      hz_issue = 1,
      hz_warning = 5,
      gap_multiplier = 3,
      gap_pct_issue = 5,
      gap_pct_warning = 2,
      largest_gap_issue_sec = 10,
      largest_gap_warning_sec = 5,
      duplicate_pct_issue = 0.01,
      coord_range_threshold = 0.0001
    )
  )

  quality_log <- list(
    initiate = list(initiate_entry)
  )

  return(quality_log)
}

#' Quality Report
#' @param .data A \code{motion_trace} object.
#' @param step Optional character vector of step name(s) to display, e.g.
#'   \code{"initiate"}, \code{c("elaborate", "allocate")}. \code{NULL}
#'   (default) shows all steps.
#' @export
quality_report <- function(.data, step = NULL){

  qual <- attr(.data, 'quality')
  meta <- attr(.data, 'metadata')

  if(is.null(qual)){
    cat("No quality log found.\n")
    return(invisible(NULL))
  }

  cat("═══════════════════════════════════════════════════════════\n")
  cat("            MOTION TRACE QUALITY REPORT                    \n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  if(!is.null(meta)){
    cat(sprintf("Session: %s\n", meta$name))
    cat(sprintf("Source:  %s\n", meta$source))
    if(!is.na(meta$player_id)){
      cat(sprintf("Player:  %s\n", meta$player_id))
    }
    cat("\n")
  }

  all_steps <- names(qual)
  cat(sprintf("Processing: %s\n\n", paste(all_steps, collapse = " → ")))

  steps <- if (!is.null(step)) {
    unknown <- setdiff(step, all_steps)
    if (length(unknown) > 0) {
      warning("quality_report(): step(s) not found in quality log: ",
              paste(unknown, collapse = ", "))
    }
    intersect(all_steps, step)
  } else {
    all_steps
  }

  if (length(steps) == 0) {
    cat("No matching steps to display.\n")
    return(invisible(qual))
  }

  # Steps that store a list of run entries (one entry appended per call).
  # filtrate / allocate / elaborate use their own nested structures.
  run_tracked <- c('initiate', 'coordinate', 'interpolate', 'derivate',
                   'designate', 'quantitate')

  for(step_name in steps){
    # Resolve the most recent entry (or use the structure directly for steps
    # that manage their own multi-entry format).
    if (step_name %in% run_tracked) {
      run_entries <- qual[[step_name]]
      n_runs      <- length(run_entries)
      s           <- run_entries[[n_runs]]
    } else {
      s      <- qual[[step_name]]
      n_runs <- 1L
    }

    cat("───────────────────────────────────────────────────────────\n")
    cat(sprintf(" %s\n", toupper(step_name)))
    cat("───────────────────────────────────────────────────────────\n")
    if (n_runs > 1L) {
      cat(sprintf("  [%d run(s) recorded — showing most recent]\n\n", n_runs))
    }

    if(step_name == "initiate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Rows:          %s\n", format(s$total_rows, big.mark = ",")))
      cat(sprintf("  Completeness:  %.1f%% (%s valid coordinates)\n",
                  s$completeness * 100,
                  format(s$valid_coordinates, big.mark = ",")))

      if(!is.na(s$native_hz)){
        cat(sprintf("  Sample Rate:   %.2f Hz (%.3f s median interval)\n",
                    s$native_hz, s$median_dt))
      }

      cat(sprintf("  Duration:      %.1f minutes\n", s$duration_sec / 60))

      if(s$n_gaps > 0){
        cat(sprintf("  Gaps:          %d (%.1f%% of intervals, largest: %.1fs)\n",
                    s$n_gaps, s$gap_percentage, s$largest_gap_sec))
      } else {
        cat("  Gaps:          None detected\n")
      }

      if(s$n_duplicates > 0){
        cat(sprintf("  Duplicates:    %d timestamps\n", s$n_duplicates))
      }

      cat("\n")

      if(s$qc_pass){
        cat("  Status: ✓ PASS\n")
      } else {
        cat("  Status: ✗ ISSUES DETECTED\n")
      }

      if(length(s$issues) > 0){
        cat("\n  Issues:\n")
        for(issue in s$issues){
          cat(sprintf("    ✗ %s\n", issue))
        }
      }

      if(length(s$warnings) > 0){
        cat("\n  Warnings:\n")
        for(warning in s$warnings){
          cat(sprintf("    ⚠ %s\n", warning))
        }
      }

      cat("\n")
    }

    else if(step_name == "coordinate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Rows:          %s\n", format(s$total_rows, big.mark = ",")))
      cat("\n")

      # Conversion path
      cat("  Conversion Path\n")
      cat(sprintf("    From:        %s\n", s$from))
      cat(sprintf("    To:          %s\n", s$to))
      if(isTRUE(s$latlong_to_xyz)){
        cat("    Method:      Lat/Lng → projected XYZ (via sf)\n")
      } else {
        cat("    Method:      XYZ passthrough\n")
      }
      cat("\n")

      # CRS
      cat("  Coordinate Reference System\n")
      if(!is.na(s$crs_code)){
        cat(sprintf("    Source CRS:  EPSG:%s (WGS 84)\n", s$crs_source))
        cat(sprintf("    Target CRS:  EPSG:%s\n", s$crs_target))
        cat(sprintf("    Description: %s\n", s$crs_description))
      } else {
        cat("    CRS:         N/A (local coordinates)\n")
      }
      cat("\n")

      # Units
      cat("  Unit Conversion\n")
      if(isTRUE(s$units_converted)){
        cat(sprintf("    Converted:   %s → %s\n", s$from_units, s$to_units))
      } else {
        cat(sprintf("    Units:       %s (no conversion applied)\n", s$to_units))
      }
      cat("\n")

      # Normalisation
      cat("  Normalisation\n")
      if(isTRUE(s$normalised)){
        cat(sprintf("    Applied:     Yes (%s)\n", s$origin_type))
      } else {
        cat("    Applied:     No\n")
      }
      cat("\n")

      # Rotation
      cat("  Rotation\n")
      if(isTRUE(s$rotated)){
        cat(sprintf("    Applied:     Yes (%.4f rad / %.2f°)\n",
                    s$rotation_angle, s$rotation_degrees))
      } else {
        cat("    Applied:     No\n")
      }
      cat("\n")

      # Z dimension
      cat("  Dimensions\n")
      cat(sprintf("    X range:     %.2f %s\n", s$x_range, s$to_units))
      cat(sprintf("    Y range:     %.2f %s\n", s$y_range, s$to_units))
      cat(sprintf("    Z data:      %s\n", if(isTRUE(s$z_present)) "Present (non-zero)" else "Absent or zero-filled"))
      cat("\n")

      # Outliers
      cat("  Spatial Outliers\n")
      if(length(s$outliers) == 0){
        cat("    None detected\n")
      } else {
        cat(sprintf("    Total flagged rows: %s (%.1f%%)\n",
                    format(s$n_outlier_rows, big.mark = ","),
                    s$n_outlier_rows / s$total_rows * 100))
        cat("\n")
        for(ol_name in names(s$outliers)){
          ol <- s$outliers[[ol_name]]
          cat(sprintf("    [%s] %s\n", toupper(ol_name), ol$description))
          cat(sprintf("      Rows affected: %s (%.1f%%)\n",
                      format(ol$n, big.mark = ","), ol$pct))
          if(!is.null(ol$bounds)){
            cat(sprintf("      Bounds:        %.2f to %.2f %s\n",
                        ol$bounds[1], ol$bounds[2], s$to_units))
          }
        }
      }
      cat("\n")

      # Dependencies
      cat("  Package Dependencies\n")
      for(pkg_name in names(s$dependencies)){
        cat(sprintf("    %-10s v%s\n", pkg_name, s$dependencies[[pkg_name]]))
      }
      cat("\n")
    }

    else if(step_name == "interpolate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Rows in:       %s\n", format(s$input_rows, big.mark = ",")))
      cat(sprintf("  Rows out:      %s\n", format(s$total_rows, big.mark = ",")))
      cat("\n")

      # Method
      cat("  Method\n")
      cat(sprintf("    Algorithm:   %s\n", s$method$algorithm))
      cat(sprintf("    Function:    %s\n", s$method$zoo_fn))
      cat(sprintf("    Description: %s\n", s$method$description))
      cat("\n")

      # Parameters
      cat("  Parameters\n")
      cat(sprintf("    Hz:              %s\n", s$parameters$hz))
      cat(sprintf("    Interval:        %s s\n", s$parameters$interval_sec))
      cat(sprintf("    Max gap frames:  %s\n", s$parameters$max_gap_frames))
      cat("\n")

      # Grid expansion
      cat("  Grid Expansion\n")
      cat(sprintf("    Duration:        %.1f s (%.1f min)\n",
                  s$duration_sec, s$duration_sec / 60))
      cat(sprintf("    Expected rows:   %s\n", format(s$expected_rows, big.mark = ",")))
      cat(sprintf("    Grid rows:       %s\n", format(s$grid_rows, big.mark = ",")))
      cat(sprintf("    Time gaps added: %s\n", format(s$time_gaps_added, big.mark = ",")))
      if(s$duplicates_removed > 0){
        cat(sprintf("    Duplicates removed: %d\n", s$duplicates_removed))
      }
      cat(sprintf("    Upstream coord NAs: %d\n", s$coord_na_from_upstream))
      cat("\n")

      # Gap structure
      cat("  Gap Structure (pre-interpolation)\n")
      if(s$gaps$n_gaps > 0){
        cat(sprintf("    Gaps:        %d\n", s$gaps$n_gaps))
        cat(sprintf("    Total NA:    %d frames\n", s$gaps$total_missing))
        cat(sprintf("    Min gap:     %d frames\n", s$gaps$min_gap))
        cat(sprintf("    Max gap:     %d frames\n", s$gaps$max_gap))
        cat(sprintf("    Mean gap:    %.2f frames\n", s$gaps$mean_gap))
        cat(sprintf("    Median gap:  %s frames\n", s$gaps$median_gap))
        if(!is.null(s$gaps$gaps_exceeding_maxgap) && s$gaps$gaps_exceeding_maxgap > 0){
          cat(sprintf("    Exceeded max_gap: %d gap(s)\n", s$gaps$gaps_exceeding_maxgap))
        }
      } else {
        cat("    No gaps detected\n")
      }
      cat("\n")

      # Interpolation results
      cat("  Results\n")
      cat(sprintf("    Interpolated:    %s rows (%.2f%%)\n",
                  format(s$n_interpolated, big.mark = ","), s$pct_interpolated))
      cat(sprintf("    Filled x:        %d\n", s$filled$x))
      cat(sprintf("    Filled y:        %d\n", s$filled$y))
      if(!is.null(s$filled$z) && s$filled$z > 0){
        cat(sprintf("    Filled z:        %d\n", s$filled$z))
      }
      if(s$remaining_na$x > 0 || s$remaining_na$y > 0){
        cat(sprintf("    Remaining NAs:   x=%d, y=%d\n",
                    s$remaining_na$x, s$remaining_na$y))
      }
      cat("\n")

      # Issues
      if(!is.null(s$issues) && length(s$issues) > 0){
        cat("  Issues:\n")
        for(issue in s$issues){
          cat(sprintf("    ⚠ %s\n", issue))
        }
        cat("\n")
      }

      # Dependencies
      cat("  Package Dependencies\n")
      for(pkg_name in names(s$dependencies)){
        cat(sprintf("    %-10s v%s\n", pkg_name, s$dependencies[[pkg_name]]))
      }
      cat("\n")
    }

    else if(step_name == "filtrate"){
      n_passes <- length(s$passes)
      cat(sprintf("  Filter passes: %d\n", n_passes))
      cat(sprintf("  Chain:         %s\n\n",
                  paste(vapply(s$passes, function(p) p$method, character(1)), collapse = " → ")))

      for(i in seq_along(s$passes)){
        p <- s$passes[[i]]
        cat(sprintf("  Pass %d: %s\n", i, toupper(p$method)))
        cat(sprintf("    Timestamp:   %s\n", .mg_fmt_ts(p$timestamp)))
        cat(sprintf("    Rows:        %s\n", format(p$total_rows, big.mark = ",")))

        # Parameters
        cat("    Parameters\n")
        for(pname in names(p$parameters)){
          cat(sprintf("      %-12s %s\n", paste0(pname, ":"), p$parameters[[pname]]))
        }

        # NAs introduced (relevant for edge-effect filters like SMA)
        total_na <- sum(p$na_introduced)
        if(total_na > 0){
          cat(sprintf("    NAs introduced: %d across %s\n",
                      total_na, paste(names(p$na_introduced), collapse = ", ")))
        } else {
          cat("    NAs introduced: None\n")
        }

        # Dependencies for this pass
        cat("    Dependencies\n")
        for(pkg_name in names(p$dependencies)){
          cat(sprintf("      %-10s v%s\n", pkg_name, p$dependencies[[pkg_name]]))
        }
        cat("\n")
      }
    }

    else if(step_name == "derivate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat("\n")

      # Coordinate source
      cat("  Coordinate Source\n")
      if(s$coordinate_source$used_filtered){
        cat(sprintf("    Used:        filtered (%s, %s)\n",
                    s$coordinate_source$x_col, s$coordinate_source$y_col))
      } else if(s$coordinate_source$requested_filtered){
        cat(sprintf("    Used:        raw (%s, %s)  [filtrate() not applied]\n",
                    s$coordinate_source$x_col, s$coordinate_source$y_col))
      } else {
        cat(sprintf("    Used:        raw (%s, %s)\n",
                    s$coordinate_source$x_col, s$coordinate_source$y_col))
      }
      cat("\n")

      # Windows
      p <- s$parameters
      cat("  Windows\n")
      cat(sprintf("    Default:          %d rows (1-second period at Hz)\n", p$window_default))
      cat(sprintf("    velocity:         %d rows\n", p$velocity))
      cat(sprintf("    acceleration:     %d rows\n", p$acceleration))
      cat(sprintf("    angular_velocity: %d rows\n", p$angular_velocity))
      cat("\n")

      # Other parameters
      cat("  Parameters\n")
      cat(sprintf("    speed_floor:  %.3f m/s\n", p$speed_floor))
      cat(sprintf("    signed_omega: %s\n", if(isTRUE(p$signed_omega)) "Yes" else "No"))
      cat("\n")

      # Output summaries
      cat("  Output Columns\n")
      for(col_name in names(s$outputs)){
        o <- s$outputs[[col_name]]
        if(!is.na(o$min)){
          cat(sprintf("    %-20s %s valid  [%.3g, %.3g]  %d NA\n",
                      col_name, format(o$n_valid, big.mark = ","),
                      o$min, o$max, o$n_na))
        } else {
          cat(sprintf("    %-20s %d NA\n", col_name, o$n_na))
        }
      }
      cat("\n")

      # Issues
      if(length(s$issues) > 0){
        cat("  Issues\n")
        for(issue in s$issues){
          cat(sprintf("    ⚠ %s\n", issue))
        }
        cat("\n")
      }
    }

    else if(step_name == "designate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat("\n")

      if(s$method == 'corbett'){
        a <- s$algorithm
        cat("  Method:  Corbett (automated change-point detection)\n\n")
        cat("  Algorithm\n")
        cat(sprintf("    Package:      changepoint v%s\n", a$package_version))
        cat(sprintf("    Function:     cpt.mean()\n"))
        cat(sprintf("    Segmentation: %s\n", a$segmentation))
        cat(sprintf("    Penalty:      %s\n", a$penalty))
        cat(sprintf("    Threshold:    %.3f m/s (active vs downtime)\n", a$v_threshold))
        cat(sprintf("    Min pts:      %d rows\n", a$min_pts))
      } else {
        cat("  Method:  Manual (CSV import)\n\n")
        cat(sprintf("    File:         %s\n", s$source_file))
        if(!is.null(s$cols_mapping)){
          if(identical(s$cols_mapping, 'guess')){
            cat("    Columns:      auto-guessed\n")
          } else if(is.character(s$cols_mapping) && !is.null(names(s$cols_mapping))){
            cat("    Column mapping:\n")
            for(k in names(s$cols_mapping)){
              cat(sprintf("      %-14s → %s\n", k, s$cols_mapping[[k]]))
            }
          }
        } else {
          cat("    Columns:      exact match (no mapping)\n")
        }
      }
      cat("\n")

      cat("  Coverage\n")
      cat(sprintf("    Rows designated: %s / %s (%.1f%%)\n",
                  format(s$n_designated, big.mark = ","),
                  format(s$n_total_rows, big.mark = ","),
                  s$pct_designated))
      cat("\n")

      cat(sprintf("  Segments (%d total)\n", s$n_segments))
      for(seg_name in names(s$segment_counts)){
        cat(sprintf("    %-20s %s rows\n",
                    seg_name,
                    format(as.integer(s$segment_counts[[seg_name]]), big.mark = ",")))
      }
      cat("\n")
    }

    else if(step_name == "allocate"){
      allocs <- s$allocations
      cat(sprintf("  Allocations applied: %d\n\n", length(allocs)))

      for(aname in names(allocs)){
        a <- allocs[[aname]]

        if(identical(a$source, 'csv')){
          cat(sprintf("  [%s]  imported from: %s\n", aname, basename(a$source_file)))
        } else {
          cat(sprintf("  [%s]  defined manually\n", aname))
        }

        for(deriv in names(a$bands)){
          b <- a$bands[[deriv]]
          cat(sprintf("    %-20s %d band%s   %s\n",
                      deriv,
                      b$n_bands,
                      if(b$n_bands == 1) "" else "s",
                      paste(b$ranges, collapse = ", ")))
        }
        cat("\n")
      }
    }

    else if(step_name == "elaborate"){
      # Each call to elaborate() appends one entry — show all of them,
      # since each represents a distinct column transformation.
      entries <- qual$elaborate
      cat(sprintf("  Calls: %d\n\n", length(entries)))

      for(i in seq_along(entries)){
        e <- entries[[i]]
        cat(sprintf("  Call %d  —  %s\n", i, .mg_fmt_ts(e$timestamp)))

        if(!is.null(e$by) && length(e$by) > 0){
          cat(sprintf("    Grouped by:  %s\n", paste(e$by, collapse = ', ')))
        } else {
          cat("    Grouped by:  (none)\n")
        }

        if(length(e$added) > 0){
          cat(sprintf("    Added:       %s\n", paste(e$added, collapse = ', ')))
        } else {
          cat("    Added:       (none)\n")
        }

        # Separate reserved overwrites from ordinary modifications
        overwritten_res <- e$overwritten_reserved %||% character(0)
        non_reserved    <- setdiff(e$modified, overwritten_res)

        if(length(non_reserved) > 0){
          cat(sprintf("    Modified:    %s\n", paste(non_reserved, collapse = ', ')))
        } else if(length(overwritten_res) == 0){
          cat("    Modified:    (none)\n")
        }

        if(length(overwritten_res) > 0){
          cat(sprintf("    ⚠ Overwrote: %s  [reserved pipeline column(s)]\n",
                      paste(overwritten_res, collapse = ', ')))
        }

        if(!is.null(e$expressions) && length(e$expressions) > 0){
          cat("    Expressions:\n")
          for(col_name in names(e$expressions)){
            cat(sprintf("      %-22s %s\n",
                        paste0(col_name, ":"), e$expressions[[col_name]]))
          }
        }

        cat("\n")
      }
    }

    else if(step_name == "quantitate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Scope:         %s\n", s$scope))
      cat(sprintf("  Allocation:    %s\n",
                  if (!is.null(s$allocation)) s$allocation else "(none)"))
      if (!is.null(s$derivative)) {
        cat(sprintf("  Derivative(s): %s\n", paste(s$derivative, collapse = ', ')))
      }
      cat(sprintf("  Rows in:       %s\n", format(s$rows_before, big.mark = ",")))
      cat(sprintf("  Rows out:      %s\n", format(s$rows_after,  big.mark = ",")))
      cat("\n")
    }
  }

  cat("═══════════════════════════════════════════════════════════\n")

  invisible(qual)
}

#' Metadata Report
#' @export
metadata_report <- function(.data){

  meta <- attr(.data, 'metadata')

  if(is.null(meta)){
    cat("No metadata found.\n")
    return(invisible(NULL))
  }

  cat("═══════════════════════════════════════════════════════════\n")
  cat("            MOTION TRACE METADATA REPORT                   \n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat("SESSION INFORMATION ────────────────────────────────────\n")
  cat(sprintf("  Name:          %s\n", meta$name))
  cat(sprintf("  Source:        %s\n", meta$source))

  if(!is.na(meta$session_id)){
    cat(sprintf("  Session ID:    %s\n", meta$session_id))
  }

  if(!is.na(meta$session_start)){
    cat(sprintf("  Date:          %s\n", as.character(meta$session_start)))
  }

  if(!is.na(meta$session_duration_sec)){
    duration_min <- meta$session_duration_sec / 60
    if(duration_min < 60){
      cat(sprintf("  Duration:      %.1f minutes\n", duration_min))
    } else {
      duration_hr <- duration_min / 60
      cat(sprintf("  Duration:      %.1f hours\n", duration_hr))
    }
  }

  cat("\n")

  has_athlete_info <- !is.na(meta$player_id) || !is.na(meta$player_name) || !is.na(meta$team)

  if(has_athlete_info){
    cat("ATHLETE INFORMATION ────────────────────────────────────\n")

    if(!is.na(meta$player_id)){
      cat(sprintf("  Player ID:     %s\n", meta$player_id))
    }

    if(!is.na(meta$player_name)){
      cat(sprintf("  Player Name:   %s\n", meta$player_name))
    }

    if(!is.na(meta$team)){
      cat(sprintf("  Team:          %s\n", meta$team))
    }

    if(!is.na(meta$sport)){
      cat(sprintf("  Sport:         %s\n", meta$sport))
    }

    if(!is.na(meta$session_type)){
      cat(sprintf("  Session Type:  %s\n", meta$session_type))
    }

    cat("\n")
  }

  has_device_info <- !is.na(meta$device_type) || !is.na(meta$device_id) ||
                     !is.na(meta$device_manufacturer)

  if(has_device_info){
    cat("DEVICE INFORMATION ─────────────────────────────────────\n")

    if(!is.na(meta$device_type)){
      cat(sprintf("  Type:          %s\n", meta$device_type))
    }

    if(!is.na(meta$device_manufacturer)){
      cat(sprintf("  Manufacturer:  %s\n", meta$device_manufacturer))
    }

    if(!is.na(meta$device_id)){
      cat(sprintf("  Device ID:     %s\n", meta$device_id))
    }

    if(!is.na(meta$firmware_version)){
      cat(sprintf("  Firmware:      %s\n", meta$firmware_version))
    }

    cat("\n")
  }

  cat("TECHNICAL DETAILS ──────────────────────────────────────\n")

  cat(sprintf("  Coordinate System:  %s\n", toupper(meta$coordinate_system)))

  if(!is.na(meta$native_hz)){
    cat(sprintf("  Sample Rate:        %.2f Hz\n", meta$native_hz))
    cat(sprintf("  Sample Interval:    %.3f seconds\n", meta$median_dt))
  }

  if(!is.null(meta$filters_applied)){
    chain <- paste(meta$filters_applied, collapse = " → ")
    cat(sprintf("  Filters Applied:    %s\n", chain))
  }

  cat("\n")

  # COLUMN MAPPING
  if(!is.null(meta$column_mapping) && meta$source %in% c('guess_csv', 'manual_csv', 'template_csv', 'gpx')){
    cat("COLUMN MAPPING ─────────────────────────────────────────\n")

    mapping <- meta$column_mapping

    for(col_name in names(mapping)){
      original <- mapping[[col_name]]
      if(!is.null(original) && original != "[not specified]"){
        cat(sprintf("  %-14s → %s\n", col_name, original))
      } else if(original == "[not specified]"){
        cat(sprintf("  %-14s → [not specified]\n", col_name))
      } else {
        cat(sprintf("  %-14s → [not found]\n", col_name))
      }
    }

    cat("\n")
  }

  cat("PROCESSING INFORMATION ─────────────────────────────────\n")
  cat(sprintf("  Created:       %s\n", .mg_fmt_ts(meta$created_timestamp)))
  cat(sprintf("  Created By:    %s\n", meta$created_by))
  cat(sprintf("  Package Ver:   %s\n", meta$package_version))
  cat("\n")

  invisible(meta)
}

#' Full Report
#' @export
full_report <- function(.data){
  cat("\n")
  metadata_report(.data)
  cat("\n")
  quality_report(.data)
  cat("\n")
  invisible(.data)
}

# segment name: print_method ---

#' Print Motion Trace
#' @export
print.motion_trace <- function(x, ...){
  meta <- attr(x, 'metadata')
  qual <- attr(x, 'quality')

  if (is.null(meta)) {
    cat(sprintf("Motion Trace [no metadata] — %s rows, %s cols\n",
                format(nrow(x), big.mark = ","), ncol(x)))
    return(invisible(x))
  }

  cat(sprintf("Motion Trace (%s)\n", meta$source))
  cat(sprintf("├─ Session: %s\n", meta$name))

  if(!is.na(meta$player_id)){
    cat(sprintf("├─ Player:  %s\n", meta$player_id))
  }

  if(!is.na(meta$native_hz)){
    cat(sprintf("├─ Sample:  %.1f Hz\n", meta$native_hz))
  }

  cat(sprintf("├─ Rows:    %s\n", format(nrow(x), big.mark = ",")))
  all_cols <- colnames(x)
  n_cols <- length(all_cols)
  if (n_cols > 15) {
    shown <- paste(all_cols[seq_len(12)], collapse = ', ')
    cat(sprintf("├─ Columns: %s ... + %d more\n", shown, n_cols - 12L))
  } else {
    cat(sprintf("├─ Columns: %s\n", paste(all_cols, collapse = ', ')))
  }

  if(!is.null(qual) && !is.null(qual$initiate)){
    latest_initiate <- qual$initiate[[length(qual$initiate)]]
    if(latest_initiate$qc_pass){
      cat("└─ QC:      ✓ Pass\n")
    } else {
      n_issues <- length(latest_initiate$issues)
      cat(sprintf("└─ QC:      ✗ %d issue%s detected\n",
                  n_issues, if(n_issues > 1) "s" else ""))
    }
  } else {
    cat("└─ QC:      Not evaluated\n")
  }

  invisible(x)
}

# segment name: initiate ---

#' Initiate a Motion Grammar Session
#'
#' @param source Character; data source. One of:
#'   \describe{
#'     \item{\code{'auto'}}{Infer source from file extension (\code{.csv} →
#'       \code{guess_csv}, \code{.gpx} → \code{gpx}).}
#'     \item{\code{'guess_csv'}}{\strong{Exploration only.} Fuzzy column
#'       detection — convenient for inspecting an unfamiliar file, but the
#'       resolved schema can change if column names or file layout change.
#'       Use \code{\link{guess_csv_template}()} to capture the detected schema
#'       and then switch to \code{'template_csv'} for reproducible pipelines.}
#'     \item{\code{'template_csv'}}{\strong{Reproducible / production.} Parses
#'       according to an explicit \code{\link{csv_template}} specification.
#'       Column mapping is locked and guaranteed to be identical across
#'       systems and time. Preferred for any analysis that will be reported
#'       or shared.}
#'     \item{\code{'manual_csv'}}{Specify column names via \code{...} args;
#'       intermediate between guessing and a saved template.}
#'     \item{\code{'strava'}}{Strava activity (requires auth).}
#'     \item{\code{'catapult_replay'}}{Catapult CSV replay export.}
#'     \item{\code{'gpx'}}{Standard GPX track file.}
#'     \item{\code{'generic'}}{Any JSON REST API via an
#'       \code{\link{api_stream_template}}.}
#'   }
#' @param session Character; Strava ID/URL or file path
#' @param coord_system Character; 'gps' or 'local'
#' @param verbose Logical; print summary
#' @param template A \code{csv_template} object created by
#'   \code{\link{csv_template}()}, or a file path to an XML template saved by
#'   \code{\link{save_csv_template}()}. When provided, \code{source} is
#'   automatically set to \code{'template_csv'}.
#' @param ... Additional arguments for manual_csv
#' @export
initiate <- function(source = 'auto',
                     session = NULL,
                     coord_system = 'gps',
                     verbose = TRUE,
                     template = NULL,
                     ...){

  if (is.null(session)) {
    stop("initiate(): 'session' must be provided. Supply a file path (CSV/GPX) or a Strava activity ID.")
  }

  # Resolve template: XML file path -> csv_template or api_stream_template
  if (is.character(template) && length(template) == 1) {
    if (!file.exists(template))
      stop("initiate(): template file not found: ", template)
    xml_content <- paste(readLines(template, warn = FALSE), collapse = "\n")
    if (grepl("<api_stream_template>", xml_content, fixed = TRUE)) {
      template <- load_api_stream_template(template)
    } else {
      template <- load_csv_template(template)
    }
  }

  # Auto-detect source
  if (!is.null(template)) {
    if (inherits(template, "csv_template")) {
      if (source != 'auto' && source != 'template_csv')
        warning("initiate(): template provided — ignoring source = '", source,
                "' and using 'template_csv'")
      source <- 'template_csv'
    } else if (inherits(template, "api_stream_template")) {
      if (source != 'auto' && source != template$api)
        warning("initiate(): template provided — ignoring source = '", source,
                "' and using '", template$api, "'")
      source <- template$api
    }
  } else if (source == 'auto' && file.exists(session)) {
    ext <- tolower(tools::file_ext(session))
    source <- switch(ext,
      'csv' = 'guess_csv',
      'gpx' = 'gpx',
      stop(sprintf("Unknown file extension: .%s\nSupported: .csv, .gpx", ext))
    )
    if (verbose) message(sprintf("Auto-detected source: %s", source))
  }

  trace <- switch(source,
    'strava' = {
      session_str <- as.character(session)
      activity_id <- if(stringr::str_detect(session_str, 'activities/')){
        stringr::str_extract(session_str, '(?<=activities/)\\d+')
      } else {
        stringr::str_extract(session_str, '^\\d+$')
      }

      api_tmpl <- if (inherits(template, "api_stream_template")) template else NULL
      tok_path <- if (!is.null(api_tmpl)) api_tmpl$token_path else '~/strava_tokens.json'
      if (!is.null(api_tmpl)) coord_system <- api_tmpl$coord_system

      tokens <- get_valid_tokens(
        jsonlite::fromJSON(path.expand(tok_path)),
        verbose = verbose
      )

      meta <- httr2::request(paste0('https://www.strava.com/api/v3/activities/', activity_id)) |>
        httr2::req_auth_bearer_token(tokens$access_token) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      trace <- get_physics_streams(activity_id, tokens$access_token, meta$start_date,
                                   template = api_tmpl)

      metadata <- create_metadata(
        session = activity_id,
        source = 'strava',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      metadata$session_id <- activity_id
      metadata$name <- meta$name
      metadata$session_start <- as.Date(lubridate::as_datetime(meta$start_date))
      metadata$session_duration_sec <- meta$elapsed_time %||% NA
      metadata$sport <- meta$sport_type %||% NA

      attr(trace, 'metadata') <- metadata
      trace
    },

    'catapult_replay' = {
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      all_lines <- readLines(session)
      header_idx <- grep('^Time, Time, Latitude', all_lines)[1]

      after_header <- all_lines[(header_idx + 1):length(all_lines)]
      empty_idx <- which(after_header == '')[1]
      n_rows <- if(is.na(empty_idx)) -1 else empty_idx - 1

      trace_raw <- readr::read_csv(
        session,
        skip = header_idx,
        n_max = n_rows,
        col_names = FALSE,
        show_col_types = FALSE,
        col_select = c(1, 3, 4)
      )

      trace <- trace_raw |>
        dplyr::rename(
          unix_raw = 1,
          lat = 2,
          lng = 3
        ) |>
        dplyr::mutate(
          unix_time = convert_to_unix(unix_raw),
          altitude = NA_real_
        ) |>
        dplyr::select(unix_time, lat, lng, altitude)

      start_line <- all_lines[grep('StartTimeSeconds', all_lines)]
      start_time_raw <- stringr::str_extract(start_line, '(?<=StartTimeSeconds ).*')

      metadata <- create_metadata(
        session = session,
        source = 'catapult_replay',
        trace = trace,
        device_info = list(type = 'Catapult'),
        coord_system = 'gps'
      )

      if(length(start_time_raw) > 0 && !is.na(start_time_raw[1])){
        parsed_start <- tryCatch({
          lubridate::parse_date_time(start_time_raw[1], orders = c('dmy HMS', 'mdy HMS'))
        }, error = function(e) NA)

        if(!is.na(parsed_start)){
          metadata$session_start <- as.Date(parsed_start)
        }
      }

      attr(trace, 'metadata') <- metadata
      trace
    },

    'guess_csv' = {
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      trace <- initiate_guess_csv(session, source = coord_system)

      # Extract column mapping from trace
      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'guess_csv',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      # Add column mapping to metadata
      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata

      # Clean up temporary attribute
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'manual_csv' = {
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      trace <- initiate_manual_csv(.data_path = session, coord_system = coord_system, ...)

      # Extract column mapping from trace
      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'manual_csv',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      # Add column mapping to metadata
      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata

      # Clean up temporary attribute
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'template_csv' = {
      if (is.null(template))
        stop("initiate(): source = 'template_csv' requires a template argument")
      if (!file.exists(session))
        stop(sprintf("File not found: %s\nCheck path and try again.", session))

      coord_system <- template$coord_system  # template takes precedence

      trace <- initiate_template_csv(session, template)

      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'template_csv',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'gpx' = {
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      trace <- initiate_gpx(session)

      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'gpx',
        trace = trace,
        device_info = NULL,
        coord_system = 'gps'
      )

      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'generic' = {
      api_tmpl <- if (inherits(template, "api_stream_template")) template else NULL
      if (is.null(api_tmpl))
        stop("initiate(): source = 'generic' requires an api_stream_template passed via template")

      coord_system <- api_tmpl$coord_system
      trace <- fetch_generic_streams(session, api_tmpl)

      metadata <- create_metadata(
        session = session,
        source = 'generic',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      attr(trace, 'metadata') <- metadata
      trace
    },

    stop("Unknown source. Use 'auto', 'strava', 'catapult_replay', 'guess_csv', 'manual_csv', 'gpx', 'template_csv', or 'generic'.")
  )

  trace$is_interpolated <- FALSE

  attr(trace, 'quality') <- init_quality_log(trace, source)

  class(trace) <- c('motion_trace', class(trace))
  if(verbose) print(trace)

  return(trace)
}


# segment name: csv_template ---

# ── XML helpers (no external dependency) ──────────────────────────────────────

#' @keywords internal
.xml_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s
}

#' @keywords internal
.xml_unescape <- function(s) {
  s <- gsub("&quot;", '"', s, fixed = TRUE)
  s <- gsub("&gt;",   ">", s, fixed = TRUE)
  s <- gsub("&lt;",   "<", s, fixed = TRUE)
  s <- gsub("&amp;",  "&", s, fixed = TRUE)
  s
}

# Parse a column reference from the XML-serialised string back to its original
# type: integer if the string is all digits, otherwise character.
#' @keywords internal
.parse_col_ref <- function(val) {
  if (is.null(val) || nchar(trimws(val)) == 0) return(NULL)
  n <- suppressWarnings(as.integer(trimws(val)))
  if (!is.na(n) && identical(trimws(val), as.character(n))) n else val
}

# ── csv_template constructor ───────────────────────────────────────────────────

#' Create a CSV Format Template
#'
#' Defines a reusable column-mapping and parsing specification for importing
#' CSV files with a known but non-standard layout. Pass the resulting object
#' to \code{\link{initiate}()} via the \code{template} argument, or serialise
#' it with \code{\link{save_csv_template}()}.
#'
#' Column references (\code{col_unix}, \code{col_lat}, etc.) accept either a
#' \strong{column name} (character, case-insensitive) or a \strong{1-based
#' integer column index}. Using an index is useful when the file has duplicate
#' column names or no header row.
#'
#' \code{col_extra} accepts a named or unnamed character or integer vector:
#' \itemize{
#'   \item \code{c("HeartRate")} — import by name; output column keeps the CSV name.
#'   \item \code{c(hr = "HeartRate")} — import by name; output column renamed to \code{hr}.
#'   \item \code{c(5L)} — import column 5 by index; output column named \code{col_5}.
#'   \item \code{c(heart_rate = 5L)} — import column 5; output column named \code{heart_rate}.
#' }
#'
#' @param coord_system Character; \code{"gps"} (lat/lng) or \code{"local"}
#'   (x/y). Defaults to \code{"gps"}.
#' @param col_unix Column name or 1-based integer index of the timestamp
#'   column. Required.
#' @param col_lat Column name or index of the latitude column
#'   (\code{coord_system = "gps"}). Required for GPS.
#' @param col_lng Column name or index of the longitude column
#'   (\code{coord_system = "gps"}). Required for GPS.
#' @param col_altitude Column name or index of the altitude column (GPS mode).
#'   Optional.
#' @param col_x Column name or index of the x-coordinate column
#'   (\code{coord_system = "local"}). Required for local.
#' @param col_y Column name or index of the y-coordinate column
#'   (\code{coord_system = "local"}). Required for local.
#' @param col_z Column name or index of the z-coordinate column (local mode).
#'   Optional.
#' @param col_velocity Column name or index of a pre-computed velocity column.
#'   When supplied it is imported as \code{velocity}, allowing you to skip
#'   \code{derivate()} for that metric.
#' @param col_acceleration Column name or index of a pre-computed acceleration
#'   column. Imported as \code{acceleration} when supplied.
#' @param col_extra Named or unnamed character or integer vector of additional
#'   columns to carry through as custom elaboration columns. See Details.
#' @param skip Integer; rows to skip \emph{before} the header row. Use this
#'   for files with device metadata or preamble above the data table. Defaults
#'   to \code{0}.
#' @param max_empty_lines Integer; consecutive blank rows that signal the end
#'   of the data block. Defaults to \code{3}.
#' @param comment Character; line-prefix marking comment rows to be ignored.
#'   Defaults to \code{"#"}.
#' @param duplicate_method Character; behaviour when a column \emph{name}
#'   matches more than one column in the file.
#'   \itemize{
#'     \item \code{"return_error"} (default) — stop with an informative message
#'       listing the duplicate positions; use an integer index instead.
#'     \item \code{"use_first"} — silently take the first matching column.
#'     \item \code{"use_second"} — take the second matching column (useful for
#'       files like Catapult where the second \code{Time} column is the GPS
#'       timestamp).
#'   }
#'
#' @return An object of class \code{csv_template}.
#' @seealso \code{\link{save_csv_template}}, \code{\link{load_csv_template}},
#'   \code{\link{initiate}}
#' @export
csv_template <- function(
    coord_system = c("gps", "local"),
    col_unix,
    col_lat = NULL,
    col_lng = NULL,
    col_altitude = NULL,
    col_x = NULL,
    col_y = NULL,
    col_z = NULL,
    col_velocity = NULL,
    col_acceleration = NULL,
    col_extra = NULL,
    skip = 0L,
    max_empty_lines = 3L,
    comment = "#",
    duplicate_method = c("return_error", "use_first", "use_second"),
    delimiter = ",",
    decimal = "."
) {
  coord_system <- match.arg(coord_system)
  duplicate_method <- match.arg(duplicate_method)

  if (!is.character(delimiter) || nchar(delimiter) != 1)
    stop("csv_template(): delimiter must be a single character (e.g. \",\", \";\", \"\\t\")")
  if (!decimal %in% c(".", ","))
    stop("csv_template(): decimal must be \".\" or \",\"")

  if (missing(col_unix) || is.null(col_unix))
    stop("csv_template(): col_unix is required")

  if (coord_system == "gps") {
    if (is.null(col_lat))
      stop("csv_template(): col_lat is required when coord_system = 'gps'")
    if (is.null(col_lng))
      stop("csv_template(): col_lng is required when coord_system = 'gps'")
    col_coord1 <- col_lat
    col_coord2 <- col_lng
    col_coord3 <- col_altitude
  } else {
    if (is.null(col_x))
      stop("csv_template(): col_x is required when coord_system = 'local'")
    if (is.null(col_y))
      stop("csv_template(): col_y is required when coord_system = 'local'")
    col_coord1 <- col_x
    col_coord2 <- col_y
    col_coord3 <- col_z
  }

  structure(
    list(
      coord_system = coord_system,
      skip = as.integer(skip),
      max_empty_lines = as.integer(max_empty_lines),
      comment = as.character(comment),
      duplicate_method = duplicate_method,
      delimiter = as.character(delimiter),
      decimal = as.character(decimal),
      col_unix = col_unix,
      col_coord1 = col_coord1,
      col_coord2 = col_coord2,
      col_coord3 = col_coord3,
      col_velocity = col_velocity,
      col_acceleration = col_acceleration,
      col_extra = col_extra
    ),
    class = "csv_template"
  )
}

#' @method print csv_template
#' @export
print.csv_template <- function(x, ...) {
  coord_names <- if (x$coord_system == "gps")
    c("lat", "lng", "altitude")
  else
    c("x", "y", "z")

  fmt_ref <- function(ref) {
    if (is.null(ref)) return(NULL)
    if (is.numeric(ref) || is.integer(ref)) sprintf("[col %d]", as.integer(ref))
    else as.character(ref)
  }

  cat(sprintf("CSV Template [%s]\n", x$coord_system))
  locale_str <- ""
  delim <- x$delimiter %||% ","
  dec   <- x$decimal   %||% "."
  if (delim != "," || dec != ".")
    locale_str <- sprintf("  |  delimiter: '%s'  |  decimal: '%s'", delim, dec)
  cat(sprintf(
    "  skip: %d  |  max_empty_lines: %d  |  comment: '%s'  |  duplicate_method: %s%s\n",
    x$skip, x$max_empty_lines, x$comment, x$duplicate_method, locale_str
  ))
  cat("  Column mapping:\n")
  cat(sprintf("    unix_time <- %s\n", fmt_ref(x$col_unix)))
  cat(sprintf("    %-12s <- %s\n", coord_names[1], fmt_ref(x$col_coord1)))
  cat(sprintf("    %-12s <- %s\n", coord_names[2], fmt_ref(x$col_coord2)))
  if (!is.null(x$col_coord3))
    cat(sprintf("    %-12s <- %s\n", coord_names[3], fmt_ref(x$col_coord3)))
  if (!is.null(x$col_velocity))
    cat(sprintf("    velocity <- %s\n", fmt_ref(x$col_velocity)))
  if (!is.null(x$col_acceleration))
    cat(sprintf("    acceleration <- %s\n", fmt_ref(x$col_acceleration)))
  if (!is.null(x$col_extra) && length(x$col_extra) > 0) {
    nms <- names(x$col_extra)
    parts <- vapply(seq_along(x$col_extra), function(i) {
      ref <- x$col_extra[[i]]
      nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL
      if (is.null(nm)) fmt_ref(ref)
      else sprintf("%s <- %s", nm, fmt_ref(ref))
    }, character(1))
    cat(sprintf("  Extra columns: %s\n", paste(parts, collapse = ", ")))
  }
  invisible(x)
}

# ── XML serialisation ──────────────────────────────────────────────────────────

#' Save a CSV Template to an XML File
#'
#' Serialises a \code{csv_template} object to a portable XML file that can be
#' shared across scripts, checked into version control, or loaded later with
#' \code{\link{load_csv_template}()}.
#'
#' @param template A \code{csv_template} object.
#' @param path Character; file path for the output \code{.xml} file.
#'
#' @return Invisibly returns \code{template}.
#' @seealso \code{\link{csv_template}}, \code{\link{load_csv_template}}
#' @export
save_csv_template <- function(template, path) {
  if (!inherits(template, "csv_template"))
    stop("save_csv_template(): template must be a csv_template object")

  add_tag <- function(name, value, indent = "  ") {
    if (is.null(value) || (length(value) == 1 && is.na(value)))
      return(character(0))
    sprintf("%s<%s>%s</%s>", indent, name, .xml_escape(as.character(value)), name)
  }

  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<csv_template>",
    add_tag("coord_system",    template$coord_system),
    add_tag("skip",            template$skip),
    add_tag("max_empty_lines", template$max_empty_lines),
    add_tag("comment",         template$comment),
    add_tag("duplicate_method", template$duplicate_method),
    add_tag("delimiter",       template$delimiter %||% ","),
    add_tag("decimal",         template$decimal   %||% "."),
    "  <columns>",
    add_tag("col_unix",         template$col_unix,         "    "),
    add_tag("col_coord1",       template$col_coord1,       "    "),
    add_tag("col_coord2",       template$col_coord2,       "    "),
    add_tag("col_coord3",       template$col_coord3,       "    "),
    add_tag("col_velocity",     template$col_velocity,     "    "),
    add_tag("col_acceleration", template$col_acceleration, "    "),
    "  </columns>"
  )

  if (!is.null(template$col_extra) && length(template$col_extra) > 0) {
    nms <- names(template$col_extra)
    extra_tags <- vapply(seq_along(template$col_extra), function(i) {
      ref <- template$col_extra[[i]]
      nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL
      as_attr <- if (!is.null(nm))
        sprintf(' as="%s"', .xml_escape(nm))
      else
        ""
      sprintf('    <col ref="%s"%s/>', .xml_escape(as.character(ref)), as_attr)
    }, character(1))
    lines <- c(lines, "  <extra_columns>", extra_tags, "  </extra_columns>")
  }

  lines <- c(lines, "</csv_template>")
  writeLines(lines, path)
  invisible(template)
}

#' Load a CSV Template from an XML File
#'
#' Reads an XML file written by \code{\link{save_csv_template}()} and returns
#' a \code{csv_template} object. The returned object can be passed directly to
#' \code{\link{initiate}()} via its \code{template} argument.
#'
#' @param path Character; path to the XML file.
#'
#' @return A \code{csv_template} object.
#' @seealso \code{\link{csv_template}}, \code{\link{save_csv_template}}
#' @export
load_csv_template <- function(path) {
  if (!file.exists(path))
    stop("load_csv_template(): file not found: ", path)

  content <- paste(readLines(path, warn = FALSE), collapse = "\n")

  get_tag <- function(tag, text) {
    pattern <- sprintf("<%s>([^<]*)</%s>", tag, tag)
    m <- regmatches(text, regexpr(pattern, text, perl = TRUE))
    if (length(m) == 0 || identical(m, character(0))) return(NULL)
    val <- sub(sprintf("^<%s>", tag), "", sub(sprintf("</%s>$", tag), "", m))
    val <- .xml_unescape(val)
    if (nchar(val) == 0) NULL else val
  }

  # Extract <columns> sub-section ((?s) = DOTALL, . matches newlines)
  cols_m <- regexpr("(?s)<columns>(.*?)</columns>", content, perl = TRUE)
  cols_section <- if (cols_m > 0) regmatches(content, cols_m) else content

  # Extract <extra_columns>
  col_extra <- NULL
  extra_m <- regexpr("(?s)<extra_columns>(.*?)</extra_columns>", content, perl = TRUE)
  if (extra_m > 0) {
    extra_section <- regmatches(content, extra_m)

    # New format: <col ref="..." as="..."/>
    ref_matches <- gregexpr('<col ref="[^"]*"[^/]*/>', extra_section, perl = TRUE)
    ref_vals <- regmatches(extra_section, ref_matches)[[1]]

    if (length(ref_vals) > 0) {
      # Extract ref attribute value via backreference
      refs <- .xml_unescape(gsub('^.*<col ref="([^"]*)".*$', "\\1", ref_vals))

      as_vals <- vapply(ref_vals, function(tag) {
        if (!grepl('as="', tag, fixed = TRUE)) return(NA_character_)
        .xml_unescape(gsub('^.*as="([^"]*)".*$', "\\1", tag))
      }, character(1))

      parsed_refs <- lapply(refs, .parse_col_ref)

      # Rebuild col_extra: named if any 'as' is present, else plain
      has_names <- any(!is.na(as_vals))
      out_names <- ifelse(is.na(as_vals), "", as_vals)

      # Detect if all refs are integer
      all_int <- all(vapply(parsed_refs, is.integer, logical(1)))
      col_extra_vec <- if (all_int)
        vapply(parsed_refs, as.integer, integer(1))
      else
        vapply(parsed_refs, function(x) as.character(x), character(1))

      if (has_names) names(col_extra_vec) <- out_names
      col_extra <- col_extra_vec

    } else {
      # Legacy format: <col>value</col>
      old_matches <- gregexpr("<col>([^<]*)</col>", extra_section, perl = TRUE)
      old_vals <- regmatches(extra_section, old_matches)[[1]]
      if (length(old_vals) > 0) {
        col_extra <- .xml_unescape(sub("^<col>", "", sub("</col>$", "", old_vals)))
      }
    }
  }

  coord_system <- get_tag("coord_system",    content) %||% "gps"
  skip <- as.integer(get_tag("skip",            content) %||% "0")
  max_empty_lines <- as.integer(get_tag("max_empty_lines", content) %||% "3")
  comment <- get_tag("comment",         content) %||% "#"
  duplicate_method <- get_tag("duplicate_method", content) %||% "return_error"
  delimiter <- get_tag("delimiter", content) %||% ","
  decimal   <- get_tag("decimal",   content) %||% "."

  # col_* values may be integer indices or column names
  col_unix <- .parse_col_ref(get_tag("col_unix",         cols_section))
  col_coord1 <- .parse_col_ref(get_tag("col_coord1",       cols_section))
  col_coord2 <- .parse_col_ref(get_tag("col_coord2",       cols_section))
  col_coord3 <- .parse_col_ref(get_tag("col_coord3",       cols_section))
  col_velocity <- .parse_col_ref(get_tag("col_velocity",     cols_section))
  col_acceleration <- .parse_col_ref(get_tag("col_acceleration", cols_section))

  if (is.null(col_unix))
    stop("load_csv_template(): <col_unix> not found in XML")
  if (is.null(col_coord1))
    stop("load_csv_template(): <col_coord1> (lat / x) not found in XML")
  if (is.null(col_coord2))
    stop("load_csv_template(): <col_coord2> (lng / y) not found in XML")

  structure(
    list(
      coord_system = coord_system,
      skip = skip,
      max_empty_lines = max_empty_lines,
      comment = comment,
      duplicate_method = duplicate_method,
      delimiter = delimiter,
      decimal = decimal,
      col_unix = col_unix,
      col_coord1 = col_coord1,
      col_coord2 = col_coord2,
      col_coord3 = col_coord3,
      col_velocity = col_velocity,
      col_acceleration = col_acceleration,
      col_extra = col_extra
    ),
    class = "csv_template"
  )
}

#' Guess a CSV Template from an Example File
#'
#' Runs the same column-detection logic as \code{source = 'guess_csv'} in
#' \code{\link{initiate}()} and converts the detected schema into a locked
#' \code{\link{csv_template}} object.
#'
#' \strong{Recommended workflow:}
#' \enumerate{
#'   \item Call \code{guess_csv_template(path, save_path = "my_device.xml")}
#'     once on a representative file to capture the resolved column mapping.
#'   \item Inspect the returned template and adjust with
#'     \code{\link{csv_template}()} if any columns were mis-detected.
#'   \item Load the saved template in all subsequent scripts with
#'     \code{initiate(template = "my_device.xml")} — column parsing is then
#'     explicit, deterministic, and independent of column-name heuristics.
#' }
#'
#' @param path Character; path to an example CSV file to inspect.
#' @param save_path Character or \code{NULL}; if supplied the template is
#'   saved to this path via \code{\link{save_csv_template}()}.
#' @param coord_system Character; \code{"auto"} (default), \code{"gps"}, or
#'   \code{"local"}.  Passed to the underlying guesser.
#'
#' @return A \code{csv_template} object.  Returned invisibly when
#'   \code{save_path} is supplied, visibly otherwise.
#' @seealso \code{\link{csv_template}}, \code{\link{save_csv_template}},
#'   \code{\link{load_csv_template}}, \code{\link{initiate}}
#' @export
guess_csv_template <- function(path, save_path = NULL, coord_system = "auto") {
  if (!file.exists(path))
    stop("guess_csv_template(): file not found: ", path)

  coord_system <- match.arg(coord_system, c("auto", "gps", "local"))

  trace <- initiate_guess_csv(path, source = coord_system)

  mapping <- attr(trace, "column_mapping")
  skip_n  <- attr(trace, "skip") %||% 0L
  cs      <- attr(trace, "coord_system_detected") %||% "gps"

  # Read the raw header row so we can detect duplicate column names and replace
  # them with integer indices (which are unambiguous).
  raw_lines  <- readLines(path, n = skip_n + 1L, warn = FALSE)
  raw_header <- raw_lines[skip_n + 1L]
  raw_cols   <- trimws(strsplit(raw_header, ",")[[1]])
  raw_cols   <- raw_cols[nchar(raw_cols) > 0]
  raw_lower  <- tolower(raw_cols)

  # Return the 1-based integer index of `name` when it appears more than once
  # in the raw header; otherwise return the name as-is (cleaner template output).
  safe_ref <- function(name) {
    if (is.null(name)) return(NULL)
    hits <- which(raw_lower == tolower(name))
    if (length(hits) > 1L) hits[1L] else name
  }

  col_unix <- safe_ref(mapping$unix_time)
  if (is.null(col_unix))
    stop("guess_csv_template(): could not detect a unix_time column — ",
         "specify column names manually with csv_template()")

  if (cs == "gps") {
    if (is.null(mapping$lat) || is.null(mapping$lng))
      stop("guess_csv_template(): could not detect lat/lng columns — ",
           "specify column names manually with csv_template()")
    tmpl <- csv_template(
      coord_system = "gps",
      col_unix     = col_unix,
      col_lat      = safe_ref(mapping$lat),
      col_lng      = safe_ref(mapping$lng),
      col_altitude = safe_ref(mapping$altitude),
      skip         = skip_n
    )
  } else {
    if (is.null(mapping$x) || is.null(mapping$y))
      stop("guess_csv_template(): could not detect x/y columns — ",
           "specify column names manually with csv_template()")
    tmpl <- csv_template(
      coord_system = "local",
      col_unix     = col_unix,
      col_x        = safe_ref(mapping$x),
      col_y        = safe_ref(mapping$y),
      col_z        = safe_ref(mapping$z),
      skip         = skip_n
    )
  }

  if (!is.null(save_path)) {
    save_csv_template(tmpl, save_path)
    message("Template saved to: ", save_path)
    return(invisible(tmpl))
  }

  tmpl
}

# ── Internal CSV reader ────────────────────────────────────────────────────────

# Resolve a column reference (string name or 1-based integer) to a column index.
# Returns an integer index into all_cols.
#' @keywords internal
.find_col_ref <- function(ref, all_cols, col_lower, duplicate_method) {
  if (is.null(ref)) return(NULL)
  n <- length(all_cols)

  if (is.numeric(ref) || is.integer(ref)) {
    idx <- as.integer(ref)
    if (idx < 1L || idx > n)
      stop(sprintf(
        "csv_template: column index %d is out of range (file has %d columns)",
        idx, n
      ))
    return(idx)
  }

  # String: case-insensitive match
  matches <- which(col_lower == stringr::str_to_lower(as.character(ref)))

  if (length(matches) == 0)
    stop(sprintf(
      "csv_template: column '%s' not found. Available: %s",
      ref, paste(all_cols, collapse = ", ")
    ))

  if (length(matches) > 1) {
    dup_info <- paste(matches, collapse = ", ")
    if (duplicate_method == "return_error")
      stop(sprintf(
        "csv_template: column name '%s' appears %d times (positions: %s). Use an integer index to select a specific column, or set duplicate_method = 'use_first' / 'use_second'.",
        ref, length(matches), dup_info
      ))
    else if (duplicate_method == "use_second") {
      if (length(matches) < 2)
        stop(sprintf(
          "csv_template: duplicate_method = 'use_second' but '%s' only appears once",
          ref
        ))
      return(matches[2])
    }
    # use_first falls through to matches[1]
  }

  matches[1]
}

#' Read a CSV file using a csv_template specification
#' @keywords internal
initiate_template_csv <- function(.data_path, template) {

  all_lines <- readr::read_lines(.data_path)

  # Skip preamble rows
  if (template$skip > 0)
    all_lines <- all_lines[(template$skip + 1L):length(all_lines)]

  # Strip comment lines
  if (nchar(template$comment) > 0)
    all_lines <- all_lines[!stringr::str_starts(
      stringr::str_trim(all_lines, side = "left"),
      stringr::fixed(template$comment)
    )]

  # Detect data end: stop at max_empty_lines consecutive blank rows
  empty_flags <- stringr::str_detect(all_lines, "^\\s*$")
  consecutive_empty <- 0L
  stop_at <- length(all_lines)

  for (i in seq_along(all_lines)) {
    if (empty_flags[i]) {
      consecutive_empty <- consecutive_empty + 1L
      if (consecutive_empty >= template$max_empty_lines) {
        stop_at <- i - template$max_empty_lines
        break
      }
    } else {
      consecutive_empty <- 0L
    }
  }

  data_lines <- all_lines[1:stop_at]
  data_lines <- data_lines[!empty_flags[1:stop_at]]

  if (length(data_lines) < 2)
    stop("csv_template: no data rows found after skip and blank-row trimming")

  delim <- template$delimiter %||% ","
  dec   <- template$decimal   %||% "."

  df <- suppressWarnings(
    readr::read_delim(
      I(paste(data_lines, collapse = "\n")),
      delim = delim,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()),
      name_repair = "minimal"   # preserve duplicate column names for duplicate_method
    )
  )

  if (nrow(df) == 0)
    stop("csv_template: no data rows found after parsing")

  # Helper: substitute decimal separator then coerce to numeric
  parse_num <- function(x) {
    if (dec != ".") x <- gsub(dec, ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }

  all_cols <- colnames(df)
  col_lower <- stringr::str_to_lower(all_cols)
  dm <- template$duplicate_method

  find <- function(ref) .find_col_ref(ref, all_cols, col_lower, dm)

  unix_idx <- find(template$col_unix)
  coord1_idx <- find(template$col_coord1)
  coord2_idx <- find(template$col_coord2)
  coord3_idx <- find(template$col_coord3)   # NULL if not specified
  vel_idx <- find(template$col_velocity)
  acc_idx <- find(template$col_acceleration)

  # Resolve col_extra: named/unnamed character or integer vector
  extra_resolved <- list()  # list of list(idx, out_name)
  if (!is.null(template$col_extra)) {
    nms <- names(template$col_extra)
    for (i in seq_along(template$col_extra)) {
      ref <- template$col_extra[[i]]
      usr_nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL

      idx <- tryCatch(find(ref), error = function(e) {
        warning(sprintf("csv_template: %s — skipping.", conditionMessage(e)))
        NULL
      })
      if (is.null(idx)) next

      # Determine output column name
      out_nm <- if (!is.null(usr_nm)) {
        usr_nm
      } else if (is.numeric(ref) || is.integer(ref)) {
        sprintf("col_%d", as.integer(ref))
      } else {
        all_cols[idx]  # preserve actual CSV case
      }

      extra_resolved <- c(extra_resolved, list(list(idx = idx, name = out_nm)))
    }
  }

  # Standard output column names
  std_names <- if (template$coord_system == "gps")
    c("unix_time", "lat", "lng", "altitude")
  else
    c("unix_time", "x", "y", "z")

  # Pre-process unix column for non-standard decimal before convert_to_unix
  unix_raw <- df[[unix_idx]]
  if (dec != ".") unix_raw <- gsub(dec, ".", unix_raw, fixed = TRUE)

  # Build output tibble with standard columns (access by integer index)
  output <- tibble::tibble(
    unix_time = convert_to_unix(unix_raw),
    coord1 = parse_num(df[[coord1_idx]]),
    coord2 = parse_num(df[[coord2_idx]]),
    coord3 = if (!is.null(coord3_idx)) parse_num(df[[coord3_idx]]) else NA_real_
  )
  colnames(output) <- std_names

  # Pre-computed velocity / acceleration
  if (!is.null(vel_idx))
    output[["velocity"]] <- parse_num(df[[vel_idx]])

  if (!is.null(acc_idx))
    output[["acceleration"]] <- parse_num(df[[acc_idx]])

  # Extra columns: try numeric, fall back to character if mostly NA
  for (er in extra_resolved) {
    raw <- df[[er$idx]]
    num_attempt <- parse_num(raw)
    output[[er$name]] <- if (mean(is.na(num_attempt)) <= 0.5) num_attempt else raw
  }

  # NA diagnostics
  n_na_unix <- sum(is.na(output[["unix_time"]]))
  if (n_na_unix > 0)
    warning(sprintf("csv_template: %d unix_time value(s) are NA", n_na_unix))

  # Column mapping for metadata (records original references)
  mapping <- list()
  mapping[["unix_time"]]  <- template$col_unix
  mapping[[std_names[2]]] <- template$col_coord1
  mapping[[std_names[3]]] <- template$col_coord2
  mapping[[std_names[4]]] <- template$col_coord3 %||% "[not specified]"
  if (!is.null(template$col_velocity))
    mapping[["velocity"]]     <- template$col_velocity
  if (!is.null(template$col_acceleration))
    mapping[["acceleration"]] <- template$col_acceleration
  if (length(extra_resolved) > 0)
    mapping[["extra_columns"]] <- vapply(extra_resolved, `[[`, character(1), "name")

  attr(output, "column_mapping") <- mapping
  output
}


# segment name: api_stream_template ---

#' Create an API Stream Template
#'
#' Defines a reusable connection, authentication, and field-mapping
#' specification for importing data from a REST JSON API. Pass the resulting
#' object to \code{\link{initiate}()} via the \code{template} argument, or
#' serialise it with \code{\link{save_api_stream_template}()}.
#'
#' \strong{Supported APIs:}
#' \itemize{
#'   \item \code{"strava"} — uses Strava's OAuth2 token-refresh flow and the
#'     \href{https://developers.strava.com/docs/reference/#api-Streams}{Streams
#'     endpoint}. Set \code{token_path} to your credentials JSON file.
#'   \item \code{"generic"} — any REST API returning JSON. Specify \code{url},
#'     \code{auth_type}, \code{auth_value}, and \code{response_format}.
#' }
#'
#' \strong{URL templates:} In \code{url}, the literal string \code{\{session\}}
#' is replaced at request time by the \code{session} argument passed to
#' \code{\link{initiate}()}. For example,
#' \code{"https://api.example.com/activities/\{session\}/data"}.
#'
#' \strong{Authentication (\code{auth_type}):}
#' \itemize{
#'   \item \code{"none"} — no credentials added.
#'   \item \code{"bearer"} — \code{Authorization: Bearer <auth_value>} header.
#'   \item \code{"api_key_header"} — \code{<auth_header>: <auth_value>} header
#'     (default header name: \code{"X-API-Key"}).
#'   \item \code{"api_key_query"} — appends \code{?<auth_param>=<auth_value>}
#'     to the URL (default param name: \code{"api_key"}).
#' }
#' If \code{auth_value} begins with \code{$} it is treated as an environment
#' variable name (e.g. \code{"$MY_API_KEY"} reads \code{Sys.getenv("MY_API_KEY")}),
#' which keeps credentials out of saved XML files.
#'
#' \strong{Response formats (\code{response_format}):}
#' \itemize{
#'   \item \code{"records"} — an array of row objects:
#'     \code{[\{"time": 0, "lat": 51.5, "lng": -0.1\}, ...]}.
#'   \item \code{"columnar"} — an object of parallel arrays:
#'     \code{\{"time": [...], "lat": [...], "lng": [...]\}}.
#'   \item \code{"streams"} — Strava-style: each key holds a sub-object whose
#'     \code{stream_data_key} entry (default \code{"data"}) is the array.
#' }
#' Use \code{data_path} (dot-separated) to navigate to the data node first,
#' e.g. \code{"result.streams"} for \code{resp$result$streams}.
#'
#' \strong{Combined lat/lng (\code{stream_latlng}):} honoured in
#' \code{"streams"} format where the API returns coordinate pairs as
#' \code{[[lat, lng], ...]}. For \code{"records"} and \code{"columnar"}
#' formats use separate \code{stream_lat}/\code{stream_lng}.
#'
#' \strong{Time formats (\code{time_type}):}
#' \itemize{
#'   \item \code{"unix"} — values are already Unix timestamps (seconds).
#'   \item \code{"iso8601"} — values are ISO 8601 strings.
#' }
#'
#' \code{stream_extra} accepts a named or unnamed character vector of
#' additional stream/field keys to carry through:
#' \itemize{
#'   \item \code{c("heartrate")} — import as-is; output column named \code{heartrate}.
#'   \item \code{c(heart_rate = "heartrate")} — import and rename to \code{heart_rate}.
#' }
#'
#' @param api Character; \code{"strava"} or \code{"generic"}.
#' @param coord_system Character; \code{"gps"} (lat/lng) or \code{"local"}
#'   (x/y). Defaults to \code{"gps"}.
#'
#' @param token_path Character; \emph{Strava only.} Path to the OAuth2
#'   credentials JSON file. Defaults to \code{"~/strava_tokens.json"}.
#'
#' @param url Character; \emph{Generic only.} Full endpoint URL. May contain
#'   \code{\{session\}} as a placeholder for the session/activity identifier
#'   supplied to \code{\link{initiate}()}. Required when
#'   \code{api = "generic"}.
#' @param request_method Character; HTTP method: \code{"GET"} (default) or
#'   \code{"POST"}.
#' @param request_params Named list; additional query parameters appended to
#'   the URL for every request.
#' @param request_body Named list; JSON body for \code{POST} requests.
#' @param auth_type Character; authentication method. One of \code{"none"},
#'   \code{"bearer"}, \code{"api_key_header"}, \code{"api_key_query"}.
#'   Defaults to \code{"none"}.
#' @param auth_value Character; token or API key. If it starts with \code{$},
#'   interpreted as an environment variable name.
#' @param auth_header Character; header name used when
#'   \code{auth_type = "api_key_header"}. Defaults to \code{"X-API-Key"}.
#' @param auth_param Character; query parameter name used when
#'   \code{auth_type = "api_key_query"}. Defaults to \code{"api_key"}.
#' @param response_format Character; JSON structure of the response:
#'   \code{"records"}, \code{"columnar"}, or \code{"streams"}. Defaults to
#'   \code{"records"}.
#' @param data_path Character; dot-separated path to the data node within the
#'   response (e.g. \code{"data"} or \code{"result.streams"}). Leave
#'   \code{NULL} to use the top-level response.
#' @param stream_data_key Character; for \code{response_format = "streams"},
#'   the key within each stream object that holds the data array. Defaults to
#'   \code{"data"}.
#' @param time_type Character; how to interpret the time stream: \code{"unix"}
#'   (already Unix seconds) or \code{"iso8601"} (ISO 8601 strings). Strava
#'   uses its own internal offset mechanism and ignores this parameter.
#'
#' @param stream_time Character; stream/field key for timestamps. Required.
#'   Defaults to \code{"time"} for Strava.
#' @param stream_latlng Character; stream key for combined lat/lng pairs
#'   (\code{"streams"} format only). Defaults to \code{"latlng"} for Strava
#'   GPS. Use \code{stream_lat}/\code{stream_lng} for separate-field APIs.
#' @param stream_lat Character; field key for latitude. Optional.
#' @param stream_lng Character; field key for longitude. Optional.
#' @param stream_altitude Character; field key for altitude. Optional.
#'   Defaults to \code{"altitude"} for Strava GPS.
#' @param stream_x Character; field key for x-coordinate (local). Required for
#'   local coordinate systems.
#' @param stream_y Character; field key for y-coordinate (local). Required for
#'   local coordinate systems.
#' @param stream_z Character; field key for z-coordinate (local). Optional.
#' @param stream_velocity Character; field key for a pre-computed velocity.
#'   Imported as \code{velocity}. Optional.
#' @param stream_acceleration Character; field key for a pre-computed
#'   acceleration. Imported as \code{acceleration}. Optional.
#' @param stream_extra Named or unnamed character vector of additional
#'   stream/field keys to carry through. See Details.
#'
#' @return An object of class \code{api_stream_template}.
#' @seealso \code{\link{save_api_stream_template}},
#'   \code{\link{load_api_stream_template}}, \code{\link{initiate}}
#' @export
api_stream_template <- function(
    api = c("strava", "generic"),
    coord_system = c("gps", "local"),
    # Strava-specific
    token_path = "~/strava_tokens.json",
    # Generic REST
    url = NULL,
    request_method = c("GET", "POST"),
    request_params = NULL,
    request_body = NULL,
    auth_type = c("none", "bearer", "api_key_header", "api_key_query"),
    auth_value = NULL,
    auth_header = "X-API-Key",
    auth_param = "api_key",
    response_format = c("records", "columnar", "streams"),
    data_path = NULL,
    stream_data_key = "data",
    time_type = c("unix", "iso8601"),
    # Stream/field mapping (all APIs)
    stream_time = NULL,
    stream_latlng = NULL,
    stream_lat = NULL,
    stream_lng = NULL,
    stream_altitude = NULL,
    stream_x = NULL,
    stream_y = NULL,
    stream_z = NULL,
    stream_velocity = NULL,
    stream_acceleration = NULL,
    stream_extra = NULL
) {
  api <- match.arg(api)
  coord_system <- match.arg(coord_system)
  request_method <- match.arg(request_method)
  auth_type <- match.arg(auth_type)
  response_format <- match.arg(response_format)
  time_type <- match.arg(time_type)

  # ── Strava defaults ──────────────────────────────────────────────────────────
  if (api == "strava") {
    if (is.null(stream_time)) stream_time <- "time"
    if (coord_system == "gps") {
      if (is.null(stream_latlng) && is.null(stream_lat) && is.null(stream_lng))
        stream_latlng <- "latlng"
      if (is.null(stream_altitude))
        stream_altitude <- "altitude"
    }
  }

  # ── Validation ───────────────────────────────────────────────────────────────
  if (is.null(stream_time) || !nchar(stream_time))
    stop("api_stream_template(): stream_time is required")

  if (api == "generic" && (is.null(url) || !nchar(url)))
    stop("api_stream_template(): url is required when api = 'generic'")

  if (auth_type != "none" && (is.null(auth_value) || !nchar(auth_value)))
    stop("api_stream_template(): auth_value is required when auth_type = '", auth_type, "'")

  if (coord_system == "gps") {
    if (is.null(stream_latlng) && (is.null(stream_lat) || is.null(stream_lng)))
      stop("api_stream_template(): coord_system = 'gps' requires stream_latlng ",
           "or both stream_lat and stream_lng")
  } else {
    if (is.null(stream_x))
      stop("api_stream_template(): coord_system = 'local' requires stream_x")
    if (is.null(stream_y))
      stop("api_stream_template(): coord_system = 'local' requires stream_y")
  }

  structure(
    list(
      api = api,
      coord_system = coord_system,
      # Strava
      token_path = as.character(token_path),
      # Generic REST
      url = if (!is.null(url)) as.character(url) else NULL,
      request_method = request_method,
      request_params = request_params,
      request_body = request_body,
      auth_type = auth_type,
      auth_value = if (!is.null(auth_value)) as.character(auth_value) else NULL,
      auth_header = as.character(auth_header),
      auth_param = as.character(auth_param),
      response_format = response_format,
      data_path = if (!is.null(data_path)) as.character(data_path) else NULL,
      stream_data_key = as.character(stream_data_key),
      time_type = time_type,
      # Field mapping
      stream_time = as.character(stream_time),
      stream_latlng = if (!is.null(stream_latlng))       as.character(stream_latlng)       else NULL,
      stream_lat = if (!is.null(stream_lat))          as.character(stream_lat)          else NULL,
      stream_lng = if (!is.null(stream_lng))          as.character(stream_lng)          else NULL,
      stream_altitude = if (!is.null(stream_altitude))     as.character(stream_altitude)     else NULL,
      stream_x = if (!is.null(stream_x))            as.character(stream_x)            else NULL,
      stream_y = if (!is.null(stream_y))            as.character(stream_y)            else NULL,
      stream_z = if (!is.null(stream_z))            as.character(stream_z)            else NULL,
      stream_velocity = if (!is.null(stream_velocity))     as.character(stream_velocity)     else NULL,
      stream_acceleration = if (!is.null(stream_acceleration)) as.character(stream_acceleration) else NULL,
      stream_extra = stream_extra
    ),
    class = "api_stream_template"
  )
}

#' @method print api_stream_template
#' @export
print.api_stream_template <- function(x, ...) {
  cat(sprintf("API Stream Template [%s / %s]\n", toupper(x$api), x$coord_system))

  if (x$api == "strava") {
    cat(sprintf("  Token path:  %s\n", x$token_path))
  } else {
    cat(sprintf("  URL:         %s\n", x$url))
    cat(sprintf("  Method:      %s\n", x$request_method))
    if (x$auth_type != "none") {
      disp_val <- if (!is.null(x$auth_value) && startsWith(x$auth_value, "$"))
        x$auth_value
      else
        "<hidden>"
      cat(sprintf("  Auth:        %s  (%s)\n", x$auth_type, disp_val))
    }
    cat(sprintf("  Response:    %s", x$response_format))
    if (!is.null(x$data_path)) cat(sprintf("  (path: %s)", x$data_path))
    cat(sprintf("  |  time_type: %s\n", x$time_type))
  }

  cat("  Stream mapping:\n")
  cat(sprintf("    unix_time <- %s\n", x$stream_time))
  if (!is.null(x$stream_latlng))
    cat(sprintf("    lat, lng <- %s (combined pairs)\n", x$stream_latlng))
  if (!is.null(x$stream_lat))  cat(sprintf("    lat <- %s\n", x$stream_lat))
  if (!is.null(x$stream_lng))  cat(sprintf("    lng <- %s\n", x$stream_lng))
  if (!is.null(x$stream_altitude))
    cat(sprintf("    altitude <- %s\n", x$stream_altitude))
  if (!is.null(x$stream_x))    cat(sprintf("    x <- %s\n", x$stream_x))
  if (!is.null(x$stream_y))    cat(sprintf("    y <- %s\n", x$stream_y))
  if (!is.null(x$stream_z))    cat(sprintf("    z <- %s\n", x$stream_z))
  if (!is.null(x$stream_velocity))
    cat(sprintf("    velocity <- %s\n", x$stream_velocity))
  if (!is.null(x$stream_acceleration))
    cat(sprintf("    acceleration <- %s\n", x$stream_acceleration))
  if (!is.null(x$stream_extra) && length(x$stream_extra) > 0) {
    nms <- names(x$stream_extra)
    parts <- vapply(seq_along(x$stream_extra), function(i) {
      key <- x$stream_extra[[i]]
      out_nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else key
      if (out_nm == key) key else sprintf("%s <- %s", out_nm, key)
    }, character(1))
    cat(sprintf("  Extra streams: %s\n", paste(parts, collapse = ", ")))
  }
  invisible(x)
}

#' Save an API Stream Template to an XML File
#'
#' Serialises an \code{api_stream_template} object to a portable XML file that
#' can be shared across scripts, checked into version control, or loaded later
#' with \code{\link{load_api_stream_template}()}.
#'
#' @param template An \code{api_stream_template} object.
#' @param path Character; file path for the output \code{.xml} file.
#'
#' @return Invisibly returns \code{template}.
#' @seealso \code{\link{api_stream_template}},
#'   \code{\link{load_api_stream_template}}
#' @export
save_api_stream_template <- function(template, path) {
  if (!inherits(template, "api_stream_template"))
    stop("save_api_stream_template(): template must be an api_stream_template object")

  add_tag <- function(name, value, indent = "  ") {
    if (is.null(value) || (length(value) == 1 && is.na(value)))
      return(character(0))
    sprintf("%s<%s>%s</%s>", indent, name, .xml_escape(as.character(value)), name)
  }

  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<api_stream_template>",
    add_tag("api",          template$api),
    add_tag("coord_system", template$coord_system),
    add_tag("token_path",   template$token_path),
    "  <request>",
    add_tag("url",            template$url,            "    "),
    add_tag("request_method", template$request_method, "    "),
    add_tag("data_path",      template$data_path,      "    "),
    "  </request>",
    "  <auth>",
    add_tag("auth_type",   template$auth_type,   "    "),
    add_tag("auth_value",  template$auth_value,  "    "),
    add_tag("auth_header", template$auth_header, "    "),
    add_tag("auth_param",  template$auth_param,  "    "),
    "  </auth>",
    "  <response>",
    add_tag("response_format", template$response_format, "    "),
    add_tag("stream_data_key", template$stream_data_key, "    "),
    add_tag("time_type",       template$time_type,       "    "),
    "  </response>",
    "  <streams>",
    add_tag("stream_time",         template$stream_time,         "    "),
    add_tag("stream_latlng",       template$stream_latlng,       "    "),
    add_tag("stream_lat",          template$stream_lat,          "    "),
    add_tag("stream_lng",          template$stream_lng,          "    "),
    add_tag("stream_altitude",     template$stream_altitude,     "    "),
    add_tag("stream_x",            template$stream_x,            "    "),
    add_tag("stream_y",            template$stream_y,            "    "),
    add_tag("stream_z",            template$stream_z,            "    "),
    add_tag("stream_velocity",     template$stream_velocity,     "    "),
    add_tag("stream_acceleration", template$stream_acceleration, "    "),
    "  </streams>"
  )

  if (!is.null(template$stream_extra) && length(template$stream_extra) > 0) {
    nms <- names(template$stream_extra)
    extra_tags <- vapply(seq_along(template$stream_extra), function(i) {
      ref <- template$stream_extra[[i]]
      nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL
      as_attr <- if (!is.null(nm)) sprintf(' as="%s"', .xml_escape(nm)) else ""
      sprintf('    <stream ref="%s"%s/>', .xml_escape(ref), as_attr)
    }, character(1))
    lines <- c(lines, "  <extra_streams>", extra_tags, "  </extra_streams>")
  }

  lines <- c(lines, "</api_stream_template>")
  writeLines(lines, path)
  invisible(template)
}

#' Load an API Stream Template from an XML File
#'
#' Reads an XML file written by \code{\link{save_api_stream_template}()} and
#' returns an \code{api_stream_template} object ready to pass to
#' \code{\link{initiate}()}.
#'
#' @param path Character; path to the XML file.
#'
#' @return An \code{api_stream_template} object.
#' @seealso \code{\link{api_stream_template}},
#'   \code{\link{save_api_stream_template}}
#' @export
load_api_stream_template <- function(path) {
  if (!file.exists(path))
    stop("load_api_stream_template(): file not found: ", path)

  content <- paste(readLines(path, warn = FALSE), collapse = "\n")

  get_tag <- function(tag, text) {
    pattern <- sprintf("<%s>([^<]*)</%s>", tag, tag)
    m <- regmatches(text, regexpr(pattern, text, perl = TRUE))
    if (length(m) == 0 || identical(m, character(0))) return(NULL)
    val <- sub(sprintf("^<%s>", tag), "", sub(sprintf("</%s>$", tag), "", m))
    val <- .xml_unescape(val)
    if (nchar(val) == 0) NULL else val
  }

  extract_section <- function(tag, text) {
    pat <- sprintf("(?s)<%s>(.*?)</%s>", tag, tag)
    m <- regexpr(pat, text, perl = TRUE)
    if (m > 0) regmatches(text, m) else text
  }

  request_section <- extract_section("request",  content)
  auth_section <- extract_section("auth",     content)
  response_section <- extract_section("response", content)
  streams_section <- extract_section("streams",  content)

  api <- get_tag("api",          content) %||% "strava"
  coord_system <- get_tag("coord_system", content) %||% "gps"
  token_path <- get_tag("token_path",   content) %||% "~/strava_tokens.json"

  url <- get_tag("url",            request_section)
  request_method <- get_tag("request_method", request_section) %||% "GET"
  data_path <- get_tag("data_path",      request_section)

  auth_type <- get_tag("auth_type",   auth_section) %||% "none"
  auth_value <- get_tag("auth_value",  auth_section)
  auth_header <- get_tag("auth_header", auth_section) %||% "X-API-Key"
  auth_param <- get_tag("auth_param",  auth_section) %||% "api_key"

  response_format <- get_tag("response_format", response_section) %||% "records"
  stream_data_key <- get_tag("stream_data_key", response_section) %||% "data"
  time_type <- get_tag("time_type",       response_section) %||% "unix"

  stream_time <- get_tag("stream_time",         streams_section)
  stream_latlng <- get_tag("stream_latlng",       streams_section)
  stream_lat <- get_tag("stream_lat",          streams_section)
  stream_lng <- get_tag("stream_lng",          streams_section)
  stream_altitude <- get_tag("stream_altitude",     streams_section)
  stream_x <- get_tag("stream_x",            streams_section)
  stream_y <- get_tag("stream_y",            streams_section)
  stream_z <- get_tag("stream_z",            streams_section)
  stream_velocity <- get_tag("stream_velocity",     streams_section)
  stream_acceleration <- get_tag("stream_acceleration", streams_section)

  if (is.null(stream_time))
    stop("load_api_stream_template(): <stream_time> not found in <streams>")

  stream_extra <- NULL
  extra_m <- regexpr("(?s)<extra_streams>(.*?)</extra_streams>", content, perl = TRUE)
  if (extra_m > 0) {
    extra_section <- regmatches(content, extra_m)
    ref_matches <- gregexpr('<stream ref="[^"]*"[^/]*/>', extra_section, perl = TRUE)
    ref_vals <- regmatches(extra_section, ref_matches)[[1]]

    if (length(ref_vals) > 0) {
      refs <- .xml_unescape(gsub('^.*<stream ref="([^"]*)".*$', "\\1", ref_vals))
      as_vals <- vapply(ref_vals, function(tag) {
        if (!grepl('as="', tag, fixed = TRUE)) return(NA_character_)
        .xml_unescape(gsub('^.*as="([^"]*)".*$', "\\1", tag))
      }, character(1))
      nms <- ifelse(is.na(as_vals), "", as_vals)
      stream_extra <- stats::setNames(refs, nms)
    }
  }

  structure(
    list(
      api = api,
      coord_system = coord_system,
      token_path = token_path,
      url = url,
      request_method = request_method,
      request_params = NULL,
      request_body = NULL,
      auth_type = auth_type,
      auth_value = auth_value,
      auth_header = auth_header,
      auth_param = auth_param,
      response_format = response_format,
      data_path = data_path,
      stream_data_key = stream_data_key,
      time_type = time_type,
      stream_time = stream_time,
      stream_latlng = stream_latlng,
      stream_lat = stream_lat,
      stream_lng = stream_lng,
      stream_altitude = stream_altitude,
      stream_x = stream_x,
      stream_y = stream_y,
      stream_z = stream_z,
      stream_velocity = stream_velocity,
      stream_acceleration = stream_acceleration,
      stream_extra = stream_extra
    ),
    class = "api_stream_template"
  )
}

#' Fetch and parse data from a generic REST JSON API
#' @keywords internal
fetch_generic_streams <- function(session, template) {

  t <- template

  # ── Build URL ─────────────────────────────────────────────────────────────
  url <- gsub("{session}", as.character(session), t$url, fixed = TRUE)

  # ── Resolve auth_value (env var support) ─────────────────────────────────
  auth_val <- t$auth_value
  if (!is.null(auth_val) && startsWith(auth_val, "$")) {
    env_var <- substring(auth_val, 2)
    auth_val <- Sys.getenv(env_var, unset = NA_character_)
    if (is.na(auth_val) || !nchar(auth_val))
      stop(sprintf("fetch_generic_streams(): environment variable '%s' is not set", env_var))
  }

  # ── Build request ─────────────────────────────────────────────────────────
  req <- httr2::request(url)

  if (t$auth_type == "bearer") {
    req <- httr2::req_auth_bearer_token(req, auth_val)
  } else if (t$auth_type == "api_key_header") {
    req <- do.call(httr2::req_headers, c(list(req), stats::setNames(list(auth_val), t$auth_header)))
  } else if (t$auth_type == "api_key_query") {
    req <- do.call(httr2::req_url_query, c(list(req), stats::setNames(list(auth_val), t$auth_param)))
  }

  if (!is.null(t$request_params) && length(t$request_params) > 0)
    req <- do.call(httr2::req_url_query, c(list(req), t$request_params))

  if (t$request_method == "POST") {
    if (!is.null(t$request_body))
      req <- httr2::req_body_json(req, t$request_body)
    req <- httr2::req_method(req, "POST")
  }

  resp <- req |> httr2::req_perform() |> httr2::resp_body_json()

  # ── Navigate to data node ─────────────────────────────────────────────────
  data <- resp
  if (!is.null(t$data_path) && nchar(t$data_path) > 0) {
    for (key in strsplit(t$data_path, ".", fixed = TRUE)[[1]]) {
      if (is.null(data[[key]]))
        stop(sprintf("fetch_generic_streams(): data_path key '%s' not found in response", key))
      data <- data[[key]]
    }
  }

  # ── Extract raw fields as character vectors ───────────────────────────────
  raw_fields <- switch(t$response_format,

    "records" = {
      if (!is.list(data) || length(data) == 0)
        stop("fetch_generic_streams(): response_format = 'records' expects a non-empty array")
      field_names <- names(data[[1]])
      result <- lapply(field_names, function(nm) {
        vapply(data, function(rec) {
          val <- rec[[nm]]
          if (is.null(val)) NA_character_ else as.character(val[[1]])
        }, character(1))
      })
      stats::setNames(result, field_names)
    },

    "columnar" = {
      lapply(data, function(v) as.character(unlist(v)))
    },

    "streams" = {
      sdk <- t$stream_data_key %||% "data"
      lapply(data, function(stream) as.character(unlist(stream[[sdk]])))
    }
  )

  n_rows <- length(raw_fields[[1]])
  output <- list()

  # ── Time ──────────────────────────────────────────────────────────────────
  time_raw <- raw_fields[[t$stream_time]]
  if (is.null(time_raw))
    stop(sprintf("fetch_generic_streams(): stream_time '%s' not found in response", t$stream_time))

  output[["unix_time"]] <- switch(t$time_type,
    "unix"    = as.numeric(time_raw),
    "iso8601" = as.numeric(lubridate::as_datetime(time_raw)),
    stop("fetch_generic_streams(): unsupported time_type '", t$time_type, "'")
  )

  # ── Coordinates ───────────────────────────────────────────────────────────
  # stream_latlng (combined pairs) only in streams/columnar where original data has pairs
  if (!is.null(t$stream_latlng) && t$response_format %in% c("streams", "columnar")) {
    pairs_src <- if (t$response_format == "streams") data[[t$stream_latlng]][[t$stream_data_key %||% "data"]]
                 else data[[t$stream_latlng]]
    if (!is.null(pairs_src)) {
      output[["lat"]] <- purrr::map_dbl(pairs_src, 1)
      output[["lng"]] <- purrr::map_dbl(pairs_src, 2)
    }
  } else {
    if (!is.null(t$stream_lat) && t$stream_lat %in% names(raw_fields))
      output[["lat"]] <- as.numeric(raw_fields[[t$stream_lat]])
    if (!is.null(t$stream_lng) && t$stream_lng %in% names(raw_fields))
      output[["lng"]] <- as.numeric(raw_fields[[t$stream_lng]])
  }

  .map_field <- function(key, out_nm) {
    if (!is.null(key) && key %in% names(raw_fields))
      output[[out_nm]] <<- as.numeric(raw_fields[[key]])
  }
  .map_field(t$stream_altitude,     "altitude")
  .map_field(t$stream_x,            "x")
  .map_field(t$stream_y,            "y")
  .map_field(t$stream_z,            "z")
  .map_field(t$stream_velocity,     "velocity")
  .map_field(t$stream_acceleration, "acceleration")

  # ── Extra streams ─────────────────────────────────────────────────────────
  if (!is.null(t$stream_extra) && length(t$stream_extra) > 0) {
    extra_nms <- names(t$stream_extra)
    for (i in seq_along(t$stream_extra)) {
      key <- t$stream_extra[[i]]
      out_nm <- if (!is.null(extra_nms) && nchar(extra_nms[i]) > 0) extra_nms[i] else key
      if (key %in% names(raw_fields)) {
        raw <- raw_fields[[key]]
        num <- suppressWarnings(as.numeric(raw))
        output[[out_nm]] <- if (mean(is.na(num)) <= 0.5) num else raw
      }
    }
  }

  # ── Fill missing core cols and assemble ───────────────────────────────────
  core_cols <- if (t$coord_system == "local") c("unix_time", "x", "y", "z")
               else c("unix_time", "lat", "lng", "altitude")

  for (col in core_cols) {
    if (is.null(output[[col]])) output[[col]] <- rep(NA_real_, n_rows)
  }

  extra_cols <- setdiff(names(output), core_cols)
  tibble::as_tibble(output[c(core_cols, extra_cols)])
}

