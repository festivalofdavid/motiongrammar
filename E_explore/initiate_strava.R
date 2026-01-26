# S) strava stuff---
# s.1 strava API auth work 
get_valid_tokens <- function(tokens, verbose = TRUE){
  now <- as.integer(Sys.time())
  
  if(!is.null(tokens$expires_at) && (tokens$expires_at - now) > 120){
    return(tokens)
  }
  
  if(verbose){
    message('Refreshing Strava tokens')
  }
  
  # RUse httr to update tokens
  resp <- httr2::request('https://www.strava.com/oauth/token') |>
    httr2::req_body_form(client_id = tokens$client_id,
      client_secret = tokens$client_secret,
      grant_type    = 'refresh_token',
      refresh_token = tokens$refresh_token) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  # Update token object
  tokens$access_token  <- resp$access_token
  tokens$refresh_token <- resp$refresh_token
  tokens$expires_at    <- resp$expires_at
  
  # Update the local cache file
  path <- path.expand('~/strava_tokens.json')
  writeLines(jsonlite::toJSON(tokens, 
    auto_unbox = TRUE,
    pretty = TRUE), 
    path
  )
  return(tokens)
}

# s.2 strava physics streams ---
get_physics_streams <- function(activity_id, access_token, start_time_iso){
  
  stream_types <- 'time,latlng,altitude'
  url <- paste0('https://www.strava.com/api/v3/activities/', 
                activity_id, 
                '/streams')
  
  resp <- httr2::request(url) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_url_query(keys = stream_types, 
                         key_by_type = 'true') |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  # Don't get unix directly as a time series, so just have to build it out myself from start time
  start_unix <- as.numeric(lubridate::as_datetime(start_time_iso))
  
  # tidy pipeline
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

# initiate_func core block ---

#' @export
initiate <- function(source = 'strava', 
                    session = '17134112147', 
                    verbose = TRUE) {
  
  if (source == 'strava') {
    session_str <- as.character(session)
    
    # Extract ID using stringr if url supplied ----
    activity_id <- if (stringr::str_detect(session_str, 'activities/')) {
      stringr::str_extract(session_str, '(?<=activities/)\\d+')
    } else {
      stringr::str_extract(session_str, '^\\d+$')
    }
    
    if (is.na(activity_id)) {
      stop('Invalid Strava input. Please provide a URL or a numeric ID.')
    }
    
    # 1. Load tokens from my local json file -----
    token_path <- path.expand('~/strava_tokens.json')
    raw_tokens <- jsonlite::fromJSON(token_path, simplifyVector = TRUE)
    tokens     <- get_valid_tokens(raw_tokens, verbose = verbose)
    
    # 2. Activity meta from strava ----
    meta <- httr2::request(paste0('https://www.strava.com/api/v3/activities/', activity_id)) |>
      httr2::req_auth_bearer_token(tokens$access_token) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
    # 3. Apply physics stream function ----
    trace <- get_physics_streams(activity_id = activity_id, 
      access_token   = tokens$access_token, 
      start_time_iso = meta$start_date)
    
    # 4. Report in console if requested ----
    if (verbose){
      cat('\n--- Session Initiated ---\n',
          'Name:', meta$name, '\n',
          'ID:  ', activity_id, '\n',
          '-------------------------\n')
    }

    # Assign metadata and class for future S3 methods
    attr(trace, 'metadata') <- meta
    class(trace) <- c('motion_trace',
                      class(trace))
    
    return(trace)
  }
  
  stop('Source not recognised.')
}


test_data <- initiate()
