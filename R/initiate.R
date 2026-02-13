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
#' @description Internal helper that auto-detects kinematic data in unstructured CSVs.
#' @param .data_path String. Path to the CSV file.
#' @param source String. Data source type: "gps" (default), "local", or "auto" to guess.
#' @param confidence_threshold Numeric. Similarity threshold (0-1) for fuzzy mapping.
#' @importFrom readr read_lines read_csv cols col_character
#' @importFrom dplyr filter group_by summarise arrange desc mutate rename select any_of all_of
#' @importFrom stringr str_count str_detect str_split str_trim str_to_lower
#' @importFrom stringdist stringdist
#' @importFrom purrr pluck discard
#' @importFrom tibble tibble
#' @keywords internal
initiate_guess_csv <- function(.data_path, 
                               source = "gps",
                               confidence_threshold = 0.7) {

  source <- match.arg(source, choices = c("gps", "local", "auto"))

  # Read file
  all_lines <- readr::read_lines(.data_path)
  comma_counts <- stringr::str_count(all_lines, ',')
  
  # Find the most frequent comma count to identify main block
  line_stats <- tibble::tibble(count = comma_counts) |>
    dplyr::filter(count > 0) |>
    dplyr::group_by(count) |>
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') |>
    dplyr::arrange(dplyr::desc(n))
  
  if (nrow(line_stats) == 0) {
    stop('No valid CSV structure detected in file.')
  }
  
  target_commas <- line_stats$count[1]
  
  # Work out header
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
  
  # Clean col names
  raw_header <- all_lines[header_idx]
  raw_cols <- raw_header |>
    stringr::str_split(',') |>
    purrr::pluck(1) |>
    stringr::str_trim()
  
  # Remove empty trailing columns
  clean_names <- raw_cols[raw_cols != '']
  
  if (length(clean_names) == 0) {
    stop('No valid column names found in header.')
  }
  
  # Fix up dupes for best practcie
  clean_names <- make.unique(clean_names, sep = '_')
  
  # Extract block of data -- most relevant to catapult replay csvs
  data_lines <- all_lines[comma_counts == target_commas]
  
  if (length(data_lines) == 0) {
    stop('No data rows found matching the identified structure.')
  }
  
  # 5. Parsing -- error handling/suppress warnings due t weird csv structure
  df <- suppressWarnings(
    readr::read_csv(
      I(paste0(c(raw_header, data_lines), collapse = '\n')),
      show_col_types = FALSE,
      name_repair = 'minimal',
      col_types = readr::cols(.default = readr::col_character())
    )
  )
  
  # Ensure we only keep cols with names
  df <- df[, 1:length(clean_names), drop = FALSE]
  colnames(df) <- clean_names
  
  # 6. auto detect coord system
  if (source == "auto") {
    col_lower <- stringr::str_to_lower(colnames(df))
    
    # Look for gps
    has_lat <- any(stringr::str_detect(col_lower, 'lat'))
    has_lon <- any(stringr::str_detect(col_lower, 'lon'))
    
    # Look for xy
    has_x <- any(col_lower == 'x' | stringr::str_detect(col_lower, 'coord_x'))
    has_y <- any(col_lower == 'y' | stringr::str_detect(col_lower, 'coord_y'))
    
    if (has_lat || has_lon) {
      source <- "gps"
    } else if (has_x || has_y) {
      source <- "local"
    } else {
      # default = gps
      source <- "gps"
    }
    
    message(sprintf("Auto-detected coordinate system: %s", source))
  }
  
  # Set some explicit targets for later fuzzy matching
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
    # Local coordinates
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
  
  # 8. Fuzzy matching
  find_best_match <- function(target_patterns, available_cols) {
    available_lower <- stringr::str_to_lower(available_cols)
    
    # Try target patterns
    for (pattern in target_patterns) {
      pattern_lower <- stringr::str_to_lower(pattern)
      
      # Exact match first
      exact_match <- which(available_lower == pattern_lower)
      if (length(exact_match) > 0) {
        return(available_cols[exact_match[1]])
      }
    }
    
    # look for exact match previously, if fails = fuzzy logic.
    best_score <- 0
    best_match <- NULL
    
    for (pattern in target_patterns) {
      pattern_lower <- stringr::str_to_lower(pattern)
      
      for (i in seq_along(available_cols)) {
        col_lower <- available_lower[i]
        
        # work out similarity
        sim <- 1 - stringdist::stringdist(pattern_lower, col_lower, method = 'jw')
        
        if (sim > best_score && sim >= confidence_threshold) {
          best_score <- sim
          best_match <- available_cols[i]
        }
      }
    }
    
    return(best_match)
  }
  
  # Apply fuzzy mappings
  mappings <- list()
  
  for (target_name in names(targets)) {

    match <- find_best_match(targets[[target_name]]$primary, colnames(df))
    
    # fallback patterns
    if (is.null(match) && !is.null(targets[[target_name]]$fallback)) {
      match <- find_best_match(targets[[target_name]]$fallback, colnames(df))
    }
    
    mappings[[target_name]] <- match
  }
  
  # 10. Rename matched columns
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

# Convert other columns to numeric
other_cols <- setdiff(output_cols, 'unix_time')
for (col in other_cols) {
  if (col %in% colnames(df)) {
    df <- df |> 
      dplyr::mutate(!!col := suppressWarnings(as.numeric(.data[[col]])))
  }
}
  
  # Select only the columns we need
  output <- df |>
    dplyr::select(dplyr::any_of(output_cols))
  
  # Add missing columns as NA
  missing_cols <- setdiff(output_cols, colnames(output))
  for (col in missing_cols) {
    output[[col]] <- NA_real_
  }
  
  # Ensure correct column order
  output <- output |>
    dplyr::select(dplyr::all_of(output_cols))
  
  # validation step
  if (nrow(output) == 0) {
    stop('No valid data rows after parsing.')
  }
  
  # Warn if all key columns are NA
  all_na <- all(sapply(output, function(x) all(is.na(x))))
  if (all_na) {
    warning('All columns are NA - column matching may have failed. Check your column names.')
  }
  
  return(output)
}

# segment name: helpers ---

#' Null Coalescing Operator
#' @keywords internal
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Create Metadata Attribute for Motion Trace
#' @description Internal helper to initialize metadata structure
#' @param session File path or session identifier
#' @param source Data source type ('strava', 'gpx', 'fit', etc.)
#' @param device_info Optional list with device details (auto-extracted if available)
#' @param coord_system Coordinate system ('gps' for lat/lng, 'local' for xy)
#' @keywords internal
create_metadata <- function(session, 
                           source, 
                           device_info = NULL,
                           coord_system = 'gps') {
  
  # Extract session name from file path or ID
  session_name <- if (file.exists(session)) {
    basename(session)
  } else {
    as.character(session)
  }
  
  # Try to extract date from filename
  date_from_name <- tryCatch({
    date_patterns <- c(
      "\\d{4}-\\d{2}-\\d{2}",  # 2024-03-27
      "\\d{8}",                 # 20240327
      "\\d{2}-\\d{2}-\\d{4}"   # 27-03-2024
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
  
  # Get package version safely
  pkg_version <- tryCatch(
    as.character(utils::packageVersion('motionGrammar')),
    error = function(e) 'dev'
  )
  
  # Build metadata structure
  metadata <- list(
    # Session identification
    name = session_name,
    source = source,
    session_id = NA_character_,
    
    # Athlete information
    player_id = NA_character_,
    player_name = NA_character_,
    team = NA_character_,
    
    # Device information
    device_type = if(!is.null(device_info$type)) device_info$type else NA_character_,
    device_id = if(!is.null(device_info$id)) device_info$id else NA_character_,
    device_manufacturer = if(!is.null(device_info$manufacturer)) device_info$manufacturer else NA_character_,
    firmware_version = if(!is.null(device_info$firmware)) device_info$firmware else NA_character_,
    
    # Session context
    sport = NA_character_,
    session_type = NA_character_,
    session_start = date_from_name,
    session_duration_sec = NA_real_,
    
    # Technical details
    coordinate_system = coord_system,
    
    # Processing metadata
    created_timestamp = Sys.time(),
    created_by = 'motionGrammar',
    package_version = pkg_version
  )
  
  return(metadata)
}

#' Update Metadata Fields
#' @description Allows users to add or modify metadata after creating a motion_trace
#' @param .data A motion_trace object
#' @param ... Named arguments to update in metadata
#' @return Motion trace with updated metadata
#' @export
#' @examples
#' trace <- initiate(...) |>
#'   set_metadata(
#'     player_id = "P001",
#'     player_name = "John Smith",
#'     sport = "football"
#'   )
set_metadata <- function(.data, ...) {
  
  if (!inherits(.data, 'motion_trace')) {
    stop("Input must be a motion_trace object")
  }
  
  meta <- attr(.data, 'metadata')
  
  if (is.null(meta)) {
    warning("No metadata found. Creating new metadata structure.")
    meta <- list()
  }
  
  # Get new metadata from ...
  new_meta <- list(...)
  
  # Update existing metadata
  for (key in names(new_meta)) {
    meta[[key]] <- new_meta[[key]]
  }
  
  attr(.data, 'metadata') <- meta
  return(.data)
}

#' Normalise Unix Timestamps to Standard Format
#' @description Detects and corrects non-standard Unix timestamp formats
#' @param unix_vec Numeric vector of Unix timestamps
#' @return Numeric vector of normalised Unix timestamps in seconds
#' @keywords internal
norm_unix <- function(unix_vec) {
  
  valid_times <- unix_vec[!is.na(unix_vec)]
  
  if (length(valid_times) == 0) {
    return(unix_vec)
  }
  
  median_time <- median(valid_times, na.rm = TRUE)
  digits <- floor(log10(abs(median_time))) + 1
  
  if (digits == 13) {
    return(unix_vec / 1000)  # Milliseconds
  } else if (digits == 11 || digits == 12) {
    return(unix_vec / 100)   # Centiseconds
  } else if (digits == 9 || digits == 10) {
    return(unix_vec)         # Already in seconds
  } else {
    warning(sprintf("Unexpected unix_time format (%d digits). Expected 9-13 digits. Returning unchanged.", digits))
    return(unix_vec)
  }
}

#' Convert Time Column to Unix Time
#' @description Converts various time formats to Unix time
#' @param time_vec Vector that might be Unix time, datetime, or frame IDs
#' @return Numeric vector in Unix time format (or frame IDs if that's all we have)
#' @keywords internal
convert_to_unix <- function(time_vec) {
  
  valid_times <- time_vec[!is.na(time_vec)]
  
  if (length(valid_times) == 0) {
    return(as.numeric(time_vec))
  }
  
  # Try converting to numeric first
  numeric_vec <- suppressWarnings(as.numeric(time_vec))
  
  # Check if conversion worked
  if (sum(!is.na(numeric_vec)) > length(valid_times) * 0.8) {
    valid_numeric <- numeric_vec[!is.na(numeric_vec)]
    median_val <- median(valid_numeric, na.rm = TRUE)
    
    if (median_val > 100000) {
      return(norm_unix(numeric_vec))  # Unix time
    } else {
      return(numeric_vec)  # Frame IDs
    }
  }
  
  # Try parsing as datetime
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

# segment name: initiate ---

#' Initiate a Motion Grammar Session
#' 
#' @param source Character; the data provider ('strava', 'catapult_replay', 'guess_csv', or 'guess_json').
#' @param session Character; the Strava ID/URL or local file path.
#' @param coord_system Character; for 'guess_csv' mode: 'gps' (default) or 'local'.
#' @param verbose Logical; if TRUE, prints a summary.
#' @return An object of class \code{motion_trace}.
#' @export
initiate <- function(source = 'strava', 
                     session = '17134112147',
                     coord_system = 'gps',
                     verbose = TRUE) {
  
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
      
      # Create metadata with Strava-specific overrides
      metadata <- create_metadata(
        session = activity_id,
        source = 'strava',
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
      if (!file.exists(session)) stop('File not found.')
      
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
      
      # Extract start time from Catapult metadata
      start_line <- all_lines[grep('StartTimeSeconds', all_lines)]
      start_time_raw <- stringr::str_extract(start_line, '(?<=StartTimeSeconds ).*')
      
      # Create metadata
      metadata <- create_metadata(
        session = session,
        source = 'catapult_replay',
        device_info = list(type = 'Catapult'),
        coord_system = 'gps'
      )
      
      # Try to parse Catapult start time
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
      if (!file.exists(session)) stop('File not found.')
      
      trace <- initiate_guess_csv(session, source = coord_system)
      
      # Create metadata
      attr(trace, 'metadata') <- create_metadata(
        session = session,
        source = 'guess_csv',
        device_info = NULL,
        coord_system = coord_system
      )
      
      trace
    },
    
    'guess_json' = {
      if (!file.exists(session)) stop('File not found.')
      stop('JSON parsing not yet implemented. Use source = "guess_csv" for CSV files.')
    },
    
    stop("Unknown source. Use 'strava', 'catapult_replay', 'guess_csv', or 'guess_json'.")
  )
  
  class(trace) <- c('motion_trace', class(trace))
  if (verbose) print(trace)
  
  return(trace)
}