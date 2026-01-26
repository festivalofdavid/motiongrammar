require('httr2')
require('jsonlite')

initial_setup <- function(auth_code, 
  your_client_id,
   your_client_secret){
  message('Exchanging code for permanent tokens...')
  resp <- request('https://www.strava.com/oauth/token') %>%
    req_body_form(
      client_id     = your_client_id,
      client_secret = your_client_secret,
      code          = auth_code,
      grant_type    = 'authorization_code'
    ) %>%
    req_perform() %>%
    resp_body_json()
  
  tokens <- list( client_id     = your_client_id,
    client_secret = your_client_secret,
    refresh_token = resp$refresh_token,
    access_token  = resp$access_token,
    expires_at    = resp$expires_at)
  
  # This creates the file on THEIR machine
  writeLines(toJSON(tokens, auto_unbox = TRUE, pretty = TRUE), 
    path.expand('~/strava_tokens.json')
  )
  message('Setup complete. You can now run the data pull script.')
}