# segment name: auth ---

#' Refresh Strava API Tokens
#'
#' @param tokens A list containing client_id, client_secret, refresh_token, and access_token.
#' @param verbose Logical; if TRUE, prints status messages.
#' @return A list of updated tokens.
#' @keywords internal
get_valid_tokens <- function(tokens,
                            verbose = TRUE) {
  now <- as.integer(Sys.time())

  if (!is.null(tokens$expires_at) && (tokens$expires_at - now) > 120) {
    return(tokens)
  }

  if (verbose) {
    message('Refreshing Strava tokens...')
  }

  resp <- httr2::request('https://www.strava.com/oauth/token') |>
    httr2::req_body_form(
      client_id     = tokens$client_id,
      client_secret = tokens$client_secret,
      grant_type    = 'refresh_token',
      refresh_token = tokens$refresh_token
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tokens$access_token  <- resp$access_token
  tokens$refresh_token <- resp$refresh_token
  tokens$expires_at    <- resp$expires_at

  path <- path.expand('~/strava_tokens.json')
  writeLines(
    jsonlite::toJSON(tokens, auto_unbox = TRUE, pretty = TRUE),
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
                               start_time_iso) {

  url <- paste0('https://www.strava.com/api/v3/activities/',
                activity_id,
                '/streams')

  resp <- httr2::request(url) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_url_query(keys = 'time,latlng,altitude',
                         key_by_type = 'true') |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  start_unix <- as.numeric(lubridate::as_datetime(start_time_iso))

  streams_df <- resp |>
    purrr::imap(\(data_list, key) {
      if (key == 'latlng') {
        tibble::tibble(
          lat = purrr::map_dbl(data_list$data, 1),
          lng = purrr::map_dbl(data_list$data, 2)
        )
      } else {
        tibble::tibble(!!key := unlist(data_list$data))
      }
    }) |>
    dplyr::bind_cols()

  final_trace <- streams_df |>
    dplyr::mutate(unix_time = start_unix + time) |>
    dplyr::select(unix_time, lat, lng, altitude)

  return(final_trace)
}

# segment name: initiate_logic ---

#' Auto-Detect and Parse Messy CSV Files
#' @keywords internal
initiate_guess_csv <- function(.data_path,
                               source = "gps",
                               confidence_threshold = 0.8) {

  source <- match.arg(source, choices = c("gps", "local", "auto"))

  all_lines <- readr::read_lines(.data_path)
  comma_counts <- stringr::str_count(all_lines, ',')

  line_stats <- tibble::tibble(count = comma_counts) |>
    dplyr::filter(count > 0) |>
    dplyr::group_by(count) |>
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') |>
    dplyr::arrange(dplyr::desc(n))

  if (nrow(line_stats) == 0) {
    stop('No valid CSV structure detected in file.')
  }

  target_commas <- line_stats$count[1]
  first_data_idx <- which(comma_counts == target_commas)[1]

  if (is.na(first_data_idx) || first_data_idx < 2) {
    stop('Could not identify data block in file.')
  }

  potential_headers <- all_lines[1:(first_data_idx - 1)]
  header_candidates <- which(stringr::str_detect(potential_headers, '[A-Za-z]'))

  if (length(header_candidates) == 0) {
    stop('Could not identify a valid header row.')
  }

  header_idx <- utils::tail(header_candidates, 1)

  raw_header <- all_lines[header_idx]
  raw_cols <- raw_header |>
    stringr::str_split(',') |>
    purrr::pluck(1) |>
    stringr::str_trim()

  clean_names <- raw_cols[raw_cols != '']

  if (length(clean_names) == 0) {
    stop('No valid column names found in header.')
  }

  clean_names <- make.unique(clean_names, sep = '_')
  data_lines <- all_lines[comma_counts == target_commas]

  if (length(data_lines) == 0) {
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

  if (source == "auto") {
    col_lower <- stringr::str_to_lower(colnames(df))

    has_lat <- any(stringr::str_detect(col_lower, 'lat'))
    has_lon <- any(stringr::str_detect(col_lower, 'lon'))
    has_x <- any(col_lower == 'x' | stringr::str_detect(col_lower, 'coord_x'))
    has_y <- any(col_lower == 'y' | stringr::str_detect(col_lower, 'coord_y'))

    if (has_lat || has_lon) {
      source <- "gps"
    } else if (has_x || has_y) {
      source <- "local"
    } else {
      source <- "gps"
    }

    message(sprintf("Auto-detected coordinate system: %s", source))
  }

  if (source == "gps") {
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

  find_best_match <- function(target_patterns, available_cols) {
    available_lower <- stringr::str_to_lower(available_cols)

    for (pattern in target_patterns) {
      pattern_lower <- stringr::str_to_lower(pattern)
      exact_match <- which(available_lower == pattern_lower)
      if (length(exact_match) > 0) {
        return(available_cols[exact_match[1]])
      }
    }

    best_score <- 0
    best_match <- NULL

    for (pattern in target_patterns) {
      pattern_lower <- stringr::str_to_lower(pattern)

      for (i in seq_along(available_cols)) {
        col_lower <- available_lower[i]
        sim <- 1 - stringdist::stringdist(pattern_lower, col_lower, method = 'jw')

        if (sim > best_score && sim >= confidence_threshold) {
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

  for (target_name in names(targets)) {
    match <- find_best_match(targets[[target_name]]$primary, available_cols)

    if (is.null(match) && !is.null(targets[[target_name]]$fallback)) {
      match <- find_best_match(targets[[target_name]]$fallback, available_cols)
    }

    mappings[[target_name]] <- match

    # Remove matched column from pool to prevent double-matching
    if (!is.null(match)) {
      available_cols <- setdiff(available_cols, match)
    }
  }

  for (target_name in names(mappings)) {
    original_col <- mappings[[target_name]]

    if (!is.null(original_col) && original_col %in% colnames(df)) {
      df <- df |>
        dplyr::rename(!!target_name := !!original_col)
    }
  }

  if ('unix_time' %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(unix_time = convert_to_unix(unix_time))
  }

  other_cols <- setdiff(output_cols, 'unix_time')
  for (col in other_cols) {
    if (col %in% colnames(df)) {
      df <- df |>
        dplyr::mutate(!!col := suppressWarnings(as.numeric(.data[[col]])))
    }
  }

  output <- df |>
    dplyr::select(dplyr::any_of(output_cols))

  missing_cols <- setdiff(output_cols, colnames(output))
  for (col in missing_cols) {
    output[[col]] <- NA_real_
  }

  output <- output |>
    dplyr::select(dplyr::all_of(output_cols))

  if (nrow(output) == 0) {
    stop('No valid data rows after parsing.')
  }

  all_na <- all(sapply(output, function(x) all(is.na(x))))
  if (all_na) {
    warning('All columns are NA - column matching may have failed. Check your column names.')
  }

  # Store column mapping as attribute for later retrieval
  attr(output, 'column_mapping') <- mappings

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
                               comment = "#") {

  coord_system <- match.arg(coord_system)

  if (!file.exists(.data_path)) {
    stop(sprintf("File not found: %s", .data_path))
  }

  if (missing(col_unix)) stop("col_unix must be specified")
  if (missing(col_lat)) stop("col_lat must be specified")
  if (missing(col_lng)) stop("col_lng must be specified")

  all_lines <- readr::read_lines(.data_path)

  if (skip > 0) {
    all_lines <- all_lines[(skip + 1):length(all_lines)]
  }

  all_lines <- all_lines[!stringr::str_detect(all_lines, sprintf("^\\s*%s", comment))]

  empty_pattern <- stringr::str_detect(all_lines, "^\\s*$")

  consecutive_empty <- 0
  stop_at <- length(all_lines)

  for (i in seq_along(all_lines)) {
    if (empty_pattern[i]) {
      consecutive_empty <- consecutive_empty + 1
      if (consecutive_empty >= max_empty_lines) {
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

  if (nrow(df) == 0) {
    stop("No data rows found after parsing")
  }

  col_names_lower <- stringr::str_to_lower(colnames(df))

  unix_idx <- which(col_names_lower == stringr::str_to_lower(col_unix))
  lat_idx <- which(col_names_lower == stringr::str_to_lower(col_lat))
  lng_idx <- which(col_names_lower == stringr::str_to_lower(col_lng))

  if (length(unix_idx) == 0) {
    stop(sprintf("Column '%s' not found. Available: %s",
                 col_unix, paste(colnames(df), collapse = ", ")))
  }

  if (length(lat_idx) == 0) {
    stop(sprintf("Column '%s' not found. Available: %s",
                 col_lat, paste(colnames(df), collapse = ", ")))
  }

  if (length(lng_idx) == 0) {
    stop(sprintf("Column '%s' not found. Available: %s",
                 col_lng, paste(colnames(df), collapse = ", ")))
  }

  alt_idx <- NULL
  if (!is.null(col_altitude)) {
    alt_idx <- which(col_names_lower == stringr::str_to_lower(col_altitude))
    if (length(alt_idx) == 0) {
      warning(sprintf("Column '%s' not found. Setting altitude to NA.", col_altitude))
    }
  }

  if (coord_system == "gps") {
    output_names <- c('unix_time', 'lat', 'lng', 'altitude')
  } else {
    output_names <- c('unix_time', 'x', 'y', 'z')
  }

  output <- tibble::tibble(
    unix_time = df[[unix_idx[1]]],
    coord1 = df[[lat_idx[1]]],
    coord2 = df[[lng_idx[1]]]
  )

  if (!is.null(alt_idx) && length(alt_idx) > 0) {
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

  if (nrow(output) == 0) {
    stop('No valid data rows after parsing')
  }

  na_unix <- sum(is.na(output$unix_time))
  na_coord1 <- sum(is.na(output[[output_names[2]]]))
  na_coord2 <- sum(is.na(output[[output_names[3]]]))

  if (na_unix > 0) {
    warning(sprintf("%d unix_time values are NA", na_unix))
  }
  if (na_coord1 > 0) {
    warning(sprintf("%d %s values are NA", na_coord1, output_names[2]))
  }
  if (na_coord2 > 0) {
    warning(sprintf("%d %s values are NA", na_coord2, output_names[3]))
  }

  # Store column mapping as attribute
  if (coord_system == "gps") {
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
initiate_gpx <- function(.data_path) {

  if (!requireNamespace("sf", quietly = TRUE)) {
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

  return(trace)
}

# segment name: helpers ---

#' Null Coalescing Operator
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Create Metadata
#' @keywords internal
create_metadata <- function(session,
                           source,
                           trace,
                           device_info = NULL,
                           coord_system = 'gps') {

  session_name <- if (file.exists(session)) {
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

    for (pattern in date_patterns) {
      match <- stringr::str_extract(session_name, pattern)
      if (!is.na(match)) {
        parsed <- lubridate::parse_date_time(match, orders = c('ymd', 'dmy'))
        if (!is.na(parsed)) return(as.Date(parsed))
      }
    }
    NA
  }, error = function(e) NA)

  time_diffs <- diff(trace$unix_time)
  time_diffs <- time_diffs[!is.na(time_diffs) & time_diffs > 0]

  if (length(time_diffs) > 0) {
    median_dt <- median(time_diffs)
    native_hz <- round(1 / median_dt, 2)
  } else {
    median_dt <- NA
    native_hz <- NA
  }

  time_range <- range(trace$unix_time, na.rm = TRUE)
  duration_sec <- if (!any(is.na(time_range))) diff(time_range) else NA

  pkg_version <- tryCatch(
    as.character(utils::packageVersion('motionGrammar')),
    error = function(e) 'dev'
  )

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
    created_by = 'motionGrammar',
    package_version = pkg_version
  )

  return(metadata)
}

#' Update Metadata
#' @export
set_metadata <- function(.data, ...) {

  if (!inherits(.data, 'motion_trace')) {
    stop("Input must be a motion_trace object")
  }

  meta <- attr(.data, 'metadata')

  if (is.null(meta)) {
    warning("No metadata found. Creating new metadata structure.")
    meta <- list()
  }

  new_meta <- list(...)

  for (key in names(new_meta)) {
    meta[[key]] <- new_meta[[key]]
  }

  attr(.data, 'metadata') <- meta
  return(.data)
}

#' Normalise Unix Timestamps
#' @keywords internal
norm_unix <- function(unix_vec) {

  valid_times <- unix_vec[!is.na(unix_vec)]

  if (length(valid_times) == 0) {
    return(unix_vec)
  }

  median_time <- median(valid_times, na.rm = TRUE)
  digits <- floor(log10(abs(median_time))) + 1

  if (digits == 13) {
    return(unix_vec / 1000)
  } else if (digits == 11 || digits == 12) {
    return(unix_vec / 100)
  } else if (digits == 9 || digits == 10) {
    return(unix_vec)
  } else {
    warning(sprintf("Unexpected unix_time format (%d digits). Expected 9-13 digits. Returning unchanged.", digits))
    return(unix_vec)
  }
}

#' Convert to Unix Time
#' @keywords internal
convert_to_unix <- function(time_vec) {

  valid_times <- time_vec[!is.na(time_vec)]

  if (length(valid_times) == 0) {
    return(as.numeric(time_vec))
  }

  numeric_vec <- suppressWarnings(as.numeric(time_vec))

  if (sum(!is.na(numeric_vec)) > length(valid_times) * 0.8) {
    valid_numeric <- numeric_vec[!is.na(numeric_vec)]
    median_val <- median(valid_numeric, na.rm = TRUE)

    if (median_val > 100000) {
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

    if (sum(!is.na(unix_time)) > length(valid_times) * 0.5) {
      return(unix_time)
    } else {
      warning("Could not parse time column as datetime. Returning as numeric.")
      return(as.numeric(time_vec))
    }
  }, error = function(e) {
    warning(sprintf("Time conversion failed: %s. Using numeric conversion.", e$message))
    return(as.numeric(time_vec))
  })
}

# segment name: quality_log ---

#' Initialize Quality Log
#' @keywords internal
init_quality_log <- function(trace, source) {

  time_diffs <- diff(trace$unix_time)
  time_diffs <- time_diffs[!is.na(time_diffs) & time_diffs > 0]

  if (length(time_diffs) > 0) {
    median_dt <- median(time_diffs)
    native_hz <- round(1 / median_dt, 2)
  } else {
    median_dt <- NA
    native_hz <- NA
  }

  if (!is.na(median_dt)) {
    gap_threshold <- median_dt * 3
    gaps <- which(time_diffs > gap_threshold)
    largest_gap <- if (length(gaps) > 0) max(time_diffs[gaps]) else 0
  } else {
    gaps <- integer(0)
    largest_gap <- 0
  }

  valid_coords <- sum(!is.na(trace$lat) & !is.na(trace$lng))
  completeness <- valid_coords / nrow(trace)

  time_range <- range(trace$unix_time, na.rm = TRUE)
  duration_sec <- diff(time_range)

  issues <- list()
  warnings <- list()

  if (completeness < 0.90) {
    issues <- c(issues, sprintf("Low coordinate completeness: %.1f%%", completeness * 100))
  } else if (completeness < 0.95) {
    warnings <- c(warnings, sprintf("Moderate missing coordinates: %.1f%%", (1 - completeness) * 100))
  }

  if (!is.na(native_hz)) {
    if (native_hz < 1) {
      issues <- c(issues, sprintf("Very low sampling rate: %.2f Hz", native_hz))
    } else if (native_hz < 5) {
      warnings <- c(warnings, sprintf("Low sampling rate: %.2f Hz (recommend ≥5 Hz)", native_hz))
    }
  }

  gap_percentage <- length(gaps) / length(time_diffs) * 100
  if (gap_percentage > 5) {
    issues <- c(issues, sprintf("High gap frequency: %d gaps (%.1f%% of intervals)",
                                length(gaps), gap_percentage))
  } else if (gap_percentage > 2) {
    warnings <- c(warnings, sprintf("Moderate gap frequency: %d gaps detected", length(gaps)))
  }

  if (largest_gap > 10) {
    issues <- c(issues, sprintf("Large temporal gap detected: %.1f seconds", largest_gap))
  } else if (largest_gap > 5) {
    warnings <- c(warnings, sprintf("Notable gap detected: %.1f seconds", largest_gap))
  }

  if (!is.na(duration_sec)) {
    if (duration_sec < 60) {
      warnings <- c(warnings, sprintf("Very short session: %.0f seconds", duration_sec))
    } else if (duration_sec > 7200) {
      warnings <- c(warnings, sprintf("Very long session: %.1f hours", duration_sec / 3600))
    }
  }

  if (source %in% c('strava', 'guess_csv', 'catapult_replay', 'manual_csv', 'gpx')) {
    lat_range <- diff(range(trace$lat, na.rm = TRUE))
    lng_range <- diff(range(trace$lng, na.rm = TRUE))

    if (lat_range < 0.0001 && lng_range < 0.0001) {
      warnings <- c(warnings, "Very small coordinate range - indoor/stationary session?")
    }

    invalid_lat <- sum(abs(trace$lat) > 90, na.rm = TRUE)
    invalid_lng <- sum(abs(trace$lng) > 180, na.rm = TRUE)

    if (invalid_lat > 0 || invalid_lng > 0) {
      issues <- c(issues, sprintf("Invalid GPS coordinates: %d latitude, %d longitude",
                                  invalid_lat, invalid_lng))
    }
  }

  n_duplicates <- sum(duplicated(trace$unix_time))
  if (n_duplicates > 0) {
    if (n_duplicates > nrow(trace) * 0.01) {
      issues <- c(issues, sprintf("High duplicate timestamps: %d (%.1f%%)",
                                  n_duplicates, n_duplicates / nrow(trace) * 100))
    } else {
      warnings <- c(warnings, sprintf("Duplicate timestamps: %d points", n_duplicates))
    }
  }

  qc_pass <- length(issues) == 0

  quality_log <- list(
    initiate = list(
      step = "initiate",
      timestamp = Sys.time(),
      source = source,

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

      lat_range = if (source != 'local') diff(range(trace$lat, na.rm = TRUE)) else NA,
      lng_range = if (source != 'local') diff(range(trace$lng, na.rm = TRUE)) else NA,

      n_duplicates = n_duplicates,

      qc_pass = qc_pass,
      issues = if (length(issues) > 0) unlist(issues) else character(0),
      warnings = if (length(warnings) > 0) unlist(warnings) else character(0)
    )
  )

  return(quality_log)
}

#' Quality Report
#' @export
quality_report <- function(.data) {

  qual <- attr(.data, 'quality')
  meta <- attr(.data, 'metadata')

  if (is.null(qual)) {
    cat("No quality log found.\n")
    return(invisible(NULL))
  }

  cat("═══════════════════════════════════════════════════════════\n")
  cat("            MOTION TRACE QUALITY REPORT                    \n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  if (!is.null(meta)) {
    cat(sprintf("Session: %s\n", meta$name))
    cat(sprintf("Source:  %s\n", meta$source))
    if (!is.na(meta$player_id)) {
      cat(sprintf("Player:  %s\n", meta$player_id))
    }
    cat("\n")
  }

  steps <- names(qual)
  cat(sprintf("Processing: %s\n\n", paste(steps, collapse = " → ")))

  for (step_name in steps) {
    s <- qual[[step_name]]

    cat("───────────────────────────────────────────────────────────\n")
    cat(sprintf(" %s\n", toupper(step_name)))
    cat("───────────────────────────────────────────────────────────\n")

    if (step_name == "initiate") {
      cat(sprintf("  Timestamp:     %s\n", format(s$timestamp, "%Y-%m-%d %H:%M:%S")))
      cat(sprintf("  Rows:          %s\n", format(s$total_rows, big.mark = ",")))
      cat(sprintf("  Completeness:  %.1f%% (%s valid coordinates)\n",
                  s$completeness * 100,
                  format(s$valid_coordinates, big.mark = ",")))

      if (!is.na(s$native_hz)) {
        cat(sprintf("  Sample Rate:   %.2f Hz (%.3f s median interval)\n",
                    s$native_hz, s$median_dt))
      }

      cat(sprintf("  Duration:      %.1f minutes\n", s$duration_sec / 60))

      if (s$n_gaps > 0) {
        cat(sprintf("  Gaps:          %d (%.1f%% of intervals, largest: %.1fs)\n",
                    s$n_gaps, s$gap_percentage, s$largest_gap_sec))
      } else {
        cat("  Gaps:          None detected\n")
      }

      if (s$n_duplicates > 0) {
        cat(sprintf("  Duplicates:    %d timestamps\n", s$n_duplicates))
      }

      cat("\n")

      if (s$qc_pass) {
        cat("  Status: ✓ PASS\n")
      } else {
        cat("  Status: ✗ ISSUES DETECTED\n")
      }

      if (length(s$issues) > 0) {
        cat("\n  Issues:\n")
        for (issue in s$issues) {
          cat(sprintf("    ✗ %s\n", issue))
        }
      }

      if (length(s$warnings) > 0) {
        cat("\n  Warnings:\n")
        for (warning in s$warnings) {
          cat(sprintf("    ⚠ %s\n", warning))
        }
      }

      cat("\n")
    }
  }

  cat("═══════════════════════════════════════════════════════════\n")

  invisible(qual)
}

#' Metadata Report
#' @export
metadata_report <- function(.data) {

  meta <- attr(.data, 'metadata')

  if (is.null(meta)) {
    cat("No metadata found.\n")
    return(invisible(NULL))
  }

  cat("═══════════════════════════════════════════════════════════\n")
  cat("            MOTION TRACE METADATA REPORT                   \n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat("┌─ SESSION INFORMATION ───────────────────────────────────┐\n")
  cat(sprintf("│ Name:          %-42s │\n", meta$name))
  cat(sprintf("│ Source:        %-42s │\n", meta$source))

  if (!is.na(meta$session_id)) {
    cat(sprintf("│ Session ID:    %-42s │\n", meta$session_id))
  }

  if (!is.na(meta$session_start)) {
    cat(sprintf("│ Date:          %-42s │\n", as.character(meta$session_start)))
  }

  if (!is.na(meta$session_duration_sec)) {
    duration_min <- meta$session_duration_sec / 60
    if (duration_min < 60) {
      cat(sprintf("│ Duration:      %.1f minutes%-30s │\n", duration_min, ""))
    } else {
      duration_hr <- duration_min / 60
      cat(sprintf("│ Duration:      %.1f hours%-32s │\n", duration_hr, ""))
    }
  }

  cat("└─────────────────────────────────────────────────────────┘\n\n")

  has_athlete_info <- !is.na(meta$player_id) || !is.na(meta$player_name) || !is.na(meta$team)

  if (has_athlete_info) {
    cat("┌─ ATHLETE INFORMATION ───────────────────────────────────┐\n")

    if (!is.na(meta$player_id)) {
      cat(sprintf("│ Player ID:     %-42s │\n", meta$player_id))
    }

    if (!is.na(meta$player_name)) {
      cat(sprintf("│ Player Name:   %-42s │\n", meta$player_name))
    }

    if (!is.na(meta$team)) {
      cat(sprintf("│ Team:          %-42s │\n", meta$team))
    }

    if (!is.na(meta$sport)) {
      cat(sprintf("│ Sport:         %-42s │\n", meta$sport))
    }

    if (!is.na(meta$session_type)) {
      cat(sprintf("│ Session Type:  %-42s │\n", meta$session_type))
    }

    cat("└─────────────────────────────────────────────────────────┘\n\n")
  }

  has_device_info <- !is.na(meta$device_type) || !is.na(meta$device_id) ||
                     !is.na(meta$device_manufacturer)

  if (has_device_info) {
    cat("┌─ DEVICE INFORMATION ────────────────────────────────────┐\n")

    if (!is.na(meta$device_type)) {
      cat(sprintf("│ Type:          %-42s │\n", meta$device_type))
    }

    if (!is.na(meta$device_manufacturer)) {
      cat(sprintf("│ Manufacturer:  %-42s │\n", meta$device_manufacturer))
    }

    if (!is.na(meta$device_id)) {
      cat(sprintf("│ Device ID:     %-42s │\n", meta$device_id))
    }

    if (!is.na(meta$firmware_version)) {
      cat(sprintf("│ Firmware:      %-42s │\n", meta$firmware_version))
    }

    cat("└─────────────────────────────────────────────────────────┘\n\n")
  }

  cat("┌─ TECHNICAL DETAILS ─────────────────────────────────────┐\n")

  cat(sprintf("│ Coordinate System:  %-35s │\n",
              toupper(meta$coordinate_system)))

  if (!is.na(meta$native_hz)) {
    cat(sprintf("│ Sample Rate:        %.2f Hz%-28s │\n",
                meta$native_hz, ""))
    cat(sprintf("│ Sample Interval:    %.3f seconds%-23s │\n",
                meta$median_dt, ""))
  }

  cat("└─────────────────────────────────────────────────────────┘\n\n")

  # COLUMN MAPPING
  if (!is.null(meta$column_mapping) && meta$source %in% c('guess_csv', 'manual_csv')) {
    cat("┌─ COLUMN MAPPING ────────────────────────────────────────┐\n")

    mapping <- meta$column_mapping

    for (col_name in names(mapping)) {
      original <- mapping[[col_name]]
      if (!is.null(original) && original != "[not specified]") {
        cat(sprintf("│ %-14s → %-38s │\n", col_name, original))
      } else if (original == "[not specified]") {
        cat(sprintf("│ %-14s → %-38s │\n", col_name, "[not specified]"))
      } else {
        cat(sprintf("│ %-14s → %-38s │\n", col_name, "[not found]"))
      }
    }

    cat("└─────────────────────────────────────────────────────────┘\n\n")
  }

  cat("┌─ PROCESSING INFORMATION ────────────────────────────────┐\n")
  cat(sprintf("│ Created:       %s%-20s │\n",
              format(meta$created_timestamp, "%Y-%m-%d %H:%M:%S"), ""))
  cat(sprintf("│ Created By:    %-42s │\n", meta$created_by))
  cat(sprintf("│ Package Ver:   %-42s │\n", meta$package_version))
  cat("└─────────────────────────────────────────────────────────┘\n\n")

  invisible(meta)
}

#' Full Report
#' @export
full_report <- function(.data) {
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
print.motion_trace <- function(x, ...) {
  meta <- attr(x, 'metadata')
  qual <- attr(x, 'quality')

  cat(sprintf("Motion Trace (%s)\n", meta$source))
  cat(sprintf("├─ Session: %s\n", meta$name))

  if (!is.na(meta$player_id)) {
    cat(sprintf("├─ Player:  %s\n", meta$player_id))
  }

  if (!is.na(meta$native_hz)) {
    cat(sprintf("├─ Sample:  %.1f Hz\n", meta$native_hz))
  }

  cat(sprintf("├─ Rows:    %s\n", format(nrow(x), big.mark = ",")))
  cat(sprintf("├─ Columns: %s\n", paste(colnames(x), collapse = ', ')))

  if (!is.null(qual) && !is.null(qual$initiate)) {
    if (qual$initiate$qc_pass) {
      cat("└─ QC:      ✓ Pass\n")
    } else {
      n_issues <- length(qual$initiate$issues)
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
#' @param source Character; data source ('auto', 'strava', 'catapult_replay', 'guess_csv', 'manual_csv', 'gpx')
#' @param session Character; Strava ID/URL or file path
#' @param coord_system Character; 'gps' or 'local'
#' @param verbose Logical; print summary
#' @param ... Additional arguments for manual_csv
#' @export
initiate <- function(source = 'auto',
                     session = '17134112147',
                     coord_system = 'gps',
                     verbose = TRUE,
                     ...) {

  # Auto-detect source from file extension
  if (source == 'auto' && file.exists(session)) {
    ext <- tolower(tools::file_ext(session))
    source <- switch(ext,
      'csv' = 'guess_csv',
      'gpx' = 'gpx',
      stop(sprintf("Unknown file extension: .%s\nSupported: .csv, .gpx", ext))
    )
    if (verbose) {
      message(sprintf("Auto-detected source: %s", source))
    }
  }

  trace <- switch(source,
    'strava' = {
      session_str <- as.character(session)
      activity_id <- if (stringr::str_detect(session_str, 'activities/')) {
        stringr::str_extract(session_str, '(?<=activities/)\\d+')
      } else {
        stringr::str_extract(session_str, '^\\d+$')
      }

      tokens <- get_valid_tokens(
        jsonlite::fromJSON(path.expand('~/strava_tokens.json')),
        verbose = verbose
      )

      meta <- httr2::request(paste0('https://www.strava.com/api/v3/activities/', activity_id)) |>
        httr2::req_auth_bearer_token(tokens$access_token) |>
        httr2::req_perform() |>
        httr2::resp_body_json()

      trace <- get_physics_streams(activity_id, tokens$access_token, meta$start_date)

      metadata <- create_metadata(
        session = activity_id,
        source = 'strava',
        trace = trace,
        device_info = NULL,
        coord_system = 'gps'
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
      if (!file.exists(session)) {
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      all_lines <- readLines(session)
      header_idx <- grep('^Time, Time, Latitude', all_lines)[1]

      after_header <- all_lines[(header_idx + 1):length(all_lines)]
      empty_idx <- which(after_header == '')[1]
      n_rows <- if (is.na(empty_idx)) -1 else empty_idx - 1

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
          lat      = 2,
          lng      = 3
        ) |>
        dplyr::mutate(
          unix_time = convert_to_unix(unix_raw),
          altitude  = NA_real_
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

      if (length(start_time_raw) > 0 && !is.na(start_time_raw[1])) {
        parsed_start <- tryCatch({
          lubridate::parse_date_time(start_time_raw[1], orders = c('dmy HMS', 'mdy HMS'))
        }, error = function(e) NA)

        if (!is.na(parsed_start)) {
          metadata$session_start <- as.Date(parsed_start)
        }
      }

      attr(trace, 'metadata') <- metadata
      trace
    },

    'guess_csv' = {
      if (!file.exists(session)) {
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
      if (!file.exists(session)) {
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

    'gpx' = {
      if (!file.exists(session)) {
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      trace <- initiate_gpx(session)

      attr(trace, 'metadata') <- create_metadata(
        session = session,
        source = 'gpx',
        trace = trace,
        device_info = NULL,
        coord_system = 'gps'
      )

      trace
    },

    stop("Unknown source. Use 'auto', 'strava', 'catapult_replay', 'guess_csv', 'manual_csv', or 'gpx'.")
  )

  attr(trace, 'quality') <- init_quality_log(trace, source)

  class(trace) <- c('motion_trace', class(trace))
  if (verbose) print(trace)

  return(trace)
}
