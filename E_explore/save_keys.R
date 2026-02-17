
library(httr2)
library(jsonlite)

token_path <- path.expand("~/strava_tokens.json")

# Fill these in:
client_id <- NA
client_secret <- NA
code <- NA

resp <- request("https://www.strava.com/oauth/token") %>%
  req_body_form(
    client_id = client_id,
    client_secret = client_secret,
    code = code,
    grant_type = "authorization_code"
  ) %>%
  req_perform()

tok <- resp_body_json(resp)

tokens <- list(
  client_id = client_id,
  client_secret = client_secret,
  access_token = tok$access_token,
  refresh_token = tok$refresh_token,
  expires_at = tok$expires_at
)

writeLines(toJSON(tokens, auto_unbox = TRUE, pretty = TRUE), token_path)
cat("Wrote fresh tokens to:", token_path, "\n")
