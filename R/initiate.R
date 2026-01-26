# segment name: auth ---

#' Refresh Strava API Tokens
#' 
#' Internal helper to check token expiry and refresh via OAuth2 if necessary.
#' 
#' @param tokens A list containing client_id, client_secret, refresh_token, and access_token.
#' @param verbose Logical; if TRUE, prints status messages to the console.
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
#' Retrieves time-series data (lat/lng, altitude, time) for a specific activity.
#' 
#' @param activity_id Character; the numeric Strava activity ID.
#' @param access_token Character; valid OAuth2 access token.
#' @param start_time_iso Character; ISO 8601 start date of the activity.
#' @return A tibble with columns: unix_time, lat, lng, altitude.
#' @keywords internal
get_physics_streams <- function(activity_id, 
                               access_token, 
                               start_time_iso) {
  
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

# segment name: initiate ---

#' Initiate a motiongrammar pipe
#' 
#' Load in our actual tracking data -- first step in any motiongrammar pipeline
#' 
#' @param source Character; the data provider (currently only 'strava').
#' @param session Character; the Strava activity ID or full URL. 
#'   Defaults to my run around central park
#' @param verbose Logical; if TRUE, prints a summary of the activity to the console.
#' @return An object of class \code{motion_trace} (a tibble with activity metadata attached).
#' @export
#' @examples
#' \dontrun{
#' trace <- initiate(session = '17134112147')
#' }
initiate <- function(source = 'strava', 
                    session = '17134112147', 
                    verbose = TRUE) {
  
  if (source == 'strava') {
    session_str <- as.character(session)
    
    activity_id <- if (stringr::str_detect(session_str, 'activities/')) {
      stringr::str_extract(session_str, '(?<=activities/)\\d+')
    } else {
      stringr::str_extract(session_str, '^\\d+$')
    }
    
    if (is.na(activity_id)) {
      stop('Invalid Strava input. Please provide a URL or a numeric ID.')
    }
    
    # 1. Load tokens
    token_path <- path.expand('~/strava_tokens.json')
    if (!file.exists(token_path)) {
      stop('Token file not found at ~/strava_tokens.json')
    }
    
    raw_tokens <- jsonlite::fromJSON(token_path, simplifyVector = TRUE)
    tokens     <- get_valid_tokens(raw_tokens, verbose = verbose)
    
    # 2. Fetch Metadata
    meta <- httr2::request(paste0('https://www.strava.com/api/v3/activities/', activity_id)) |>
      httr2::req_auth_bearer_token(tokens$access_token) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
    # 3. Pull Streams
    trace <- get_physics_streams(
      activity_id    = activity_id, 
      access_token   = tokens$access_token, 
      start_time_iso = meta$start_date
    )
    
    # Reporting
    if (verbose) {
      sport_type <- if (!is.null(meta$sport_type)) meta$sport_type else 'Activity'
      
      cat('\n--- Session Initiated ---\n',
          'Name:  ', meta$name, '\n',
          'Sport: ', sport_type, '\n',
          'ID:    ', activity_id, '\n',
          '-------------------------\n')
    }

    attr(trace, 'metadata') <- meta
    class(trace) <- c('motion_trace', class(trace))
    
    return(trace)
  }
  
  stop('Source not recognised.')
}