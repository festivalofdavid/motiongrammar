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

# segment name: methods ---

#' Print Method for motion_trace
#' 
#' @param x An object of class \code{motion_trace}.
#' @param ... Additional arguments.
#' @export
print.motion_trace <- function(x, ...) {
  meta <- attr(x, 'metadata')
  
  cat('\n--- Motion Grammar Trace ---\n')
  cat('Name:   ', meta$name, '\n')
  cat('Source: ', meta$source, '\n')
  cat('Points: ', nrow(x), '\n')
  cat('----------------------------\n\n')
  
  NextMethod()
}

# segment name: initiate ---

#' Initiate a Motion Grammar Session
#' 
#' @param source Character; the data provider ('strava' or 'catapult_replay').
#' @param session Character; the Strava ID/URL or local file path.
#' @param verbose Logical; if TRUE, prints a summary.
#' @return An object of class \code{motion_trace}.
#' @export
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
    
    tokens <- get_valid_tokens(
      jsonlite::fromJSON(path.expand('~/strava_tokens.json')), 
      verbose = verbose
    )
    
    meta <- httr2::request(paste0('https://www.strava.com/api/v3/activities/', activity_id)) |>
      httr2::req_auth_bearer_token(tokens$access_token) |>
      httr2::req_perform() |>
      httr2::resp_body_json()
    
    trace <- get_physics_streams(activity_id, tokens$access_token, meta$start_date)
    
    attr(trace, 'metadata') <- list(
      name = meta$name, 
      source = 'strava'
    )
  } 
  
  else if (source == 'catapult_replay') {
    if (!file.exists(session)) stop('File not found.')
    
    all_lines <- readLines(session)
    header_idx <- grep('^Time, Time, Latitude', all_lines)[1]
    
    # Catapults csvs have this junk at the bottom, so code below exists to scrub it out.
    # Calculate rows until end of GPS block
    after_header <- all_lines[(header_idx + 1):length(all_lines)]
    empty_idx <- which(after_header == '')[1]
    n_rows <- if (is.na(empty_idx)) -1 else empty_idx - 1

    # Use col_names = FALSE to bypass the duplicate "Time" header error
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
        unix_time = unix_raw / 100, 
        altitude  = NA_real_
      ) |>
      dplyr::select(unix_time, lat, lng, altitude)
    
    start_line <- all_lines[grep('StartTimeSeconds', all_lines)]
    attr(trace, 'metadata') <- list(
      name = basename(session), 
      source = 'catapult_replay',
      start_time_raw = stringr::str_extract(start_line, '(?<=StartTimeSeconds ).*')
    )
  }
  
  class(trace) <- c('motion_trace', class(trace))
  if (verbose) print(trace)
  
  return(trace)
}

dat <- initiate(source = 'catapult_replay',
session = '/Users/david/Downloads/Adams_25_50657_13_10_09_.csv')
