
token_path <- path.expand('~/strava_tokens.json')
read_tokens <- function() fromJSON(token_path, simplifyVector = TRUE)

get_valid_tokens <- function(tokens, leeway_sec = 120) {
  now <- as.integer(Sys.time())
  if (!is.null(tokens$expires_at) && (tokens$expires_at - now) > leeway_sec) {
    return(tokens)
  }
  
  # Refresh logic
  resp <- request('https://www.strava.com/oauth/token') %>%
    req_body_form(
      client_id     = tokens$client_id,
      client_secret = tokens$client_secret,
      grant_type    = 'refresh_token',
      refresh_token = tokens$refresh_token
    ) %>%
    req_perform() %>%
    resp_body_json()
  
  tokens$access_token  <- resp$access_token
  tokens$refresh_token <- resp$refresh_token
  tokens$expires_at    <- resp$expires_at
  
  writeLines(toJSON(tokens, auto_unbox = TRUE, pretty = TRUE), token_path)
  tokens
}