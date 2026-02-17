library(httr2)
library(jsonlite)
library(tidyverse)

token_path <- path.expand("~/strava_tokens.json")
read_tokens <- function() fromJSON(token_path, simplifyVector = TRUE)
write_tokens <- function(x) writeLines(toJSON(x, auto_unbox = TRUE, pretty = TRUE), token_path)

refresh_tokens <- function(tokens) {
  # Strava refresh endpoint (doc'd)
  resp <- request("https://www.strava.com/oauth/token") %>%
    req_body_form(
      client_id     = tokens$client_id,
      client_secret = tokens$client_secret,
      grant_type    = "refresh_token",
      refresh_token = tokens$refresh_token
    ) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()

  if (resp_status(resp) >= 400) {
    stop(paste("Refresh failed:", resp_body_string(resp)), call. = FALSE)
  }

  r <- resp_body_json(resp)

  # Rolling refresh tokens: persist the most recent refresh_token
  tokens$access_token  <- r$access_token
  tokens$refresh_token <- r$refresh_token
  tokens$expires_at    <- r$expires_at

  write_tokens(tokens)
  tokens
}

get_valid_tokens <- function(tokens, leeway_sec = 120) {
  now <- as.integer(Sys.time())

  ok_access  <- !is.null(tokens$access_token) && nzchar(tokens$access_token)
  ok_expiry  <- !is.null(tokens$expires_at) && is.finite(tokens$expires_at)

  if (ok_access && ok_expiry && (tokens$expires_at - now) > leeway_sec) {
    return(tokens)
  }
  refresh_tokens(tokens)
}

# ---- Example: pull latest 5 activities ----
tokens <- get_valid_tokens(read_tokens())

activities <- request("https://www.strava.com/api/v3/athlete/activities") %>%
  req_auth_bearer_token(tokens$access_token) %>%
  req_url_query(per_page = 5, page = 1) %>%
  req_perform() %>%
  resp_body_json() %>%
  tibble(activity = .) %>%
  tidyr::unnest_wider(activity)

activities %>% select(name, distance, start_date_local) %>% print(n = Inf)


#### Get activity streams
# segment name: fetch_streams_fixed ---

get_activity_streams <- function(activity_id, access_token) {
  
  # Requesting specific stream types
  stream_types <- 'time,latlng,distance,altitude,velocity_smooth,heartrate,cadence,watts'
  url <- paste0('https://www.strava.com/api/v3/activities/', activity_id, '/streams')
  
  resp <- request(url) %>%
    req_auth_bearer_token(access_token) %>%
    req_url_query(keys = stream_types, key_by_type = 'true') %>%
    req_perform() %>%
    resp_body_json()
  
  # We use imap to iterate over the list AND its names (e.g., 'altitude', 'latlng')
  streams_df <- resp %>%
    imap(~ {
      if (.y == 'latlng') {
        # 'latlng' is a list of pairs: [[lat, lng], [lat, lng]]
        tibble(
          lat = map_dbl(.x$data, 1),
          lng = map_dbl(.x$data, 2)
        )
      } else {
        # All other types are simple vectors
        tibble(!!.y := unlist(.x$data))
      }
    }) %>%
    # bind_cols works here because Strava streams are always the same length
    bind_cols()
  
  return(streams_df)
}

# segment name: execution ---

tokens <- get_valid_tokens(read_tokens())

# Grab the ID of the first activity in your 'activities' tibble
latest_id <- activities$id[1]

time_series <- get_activity_streams(latest_id, tokens$access_token)

# segment name: display ---
print(head(time_series))

ggplot(time_series, aes(x = distance / 1000, y = altitude)) +
  geom_area(fill = '#0073C2FF', alpha = 0.4) +
  geom_line(color = '#0073C2FF', size = 1) +
  labs(
    title = 'Activity Elevation Profile',
    x = 'Distance (km)',
    y = 'Altitude (m)'
  ) +
  theme_minimal()