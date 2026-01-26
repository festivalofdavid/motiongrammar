require(httr2)
require(tidyverse)

key_path <- '/Users/david/Documents/motiongrammar/secrets.txt'
keys <- readLines(key_path)%>%
  strsplit(': ') %>%
  map(~ set_names(.x[2], .x[1])) %>%
  flatten() %>%
  map_chr(trimws)

req <- request('https://www.strava.com/api/v3/athlete') %>%
  req_auth_bearer_token(keys['access_token'])
resp <- req_perform(req)

status <- resp_status(resp)

if (status == 200) {
  print('Success! Connection established.')
  athlete <- resp_body_json(resp)
  cat('Logged in as:', athlete$firstname, athlete$lastname)
} else if (status == 401) {
  print('Unauthorized: Your access_token has likely expired.')
} else {
  cat('Error:', status)
}

# 1. Build the request to list activities
list_req <- request('https://www.strava.com/api/v3/athlete/activities') %>%
  req_url_query(per_page = 5) %>% # Get the 5 most recent
  req_headers(Authorization = paste('Bearer', keys['access_token']))

# 2. Perform and parse
list_resp <- req_perform(list_req)
activities_raw <- resp_body_json(list_resp)

# 3. Flatten into a scannable tibble
activities_df <- activities_raw %>%
  map_df(~ tibble(
    id = .x$id,
    name = .x$name,
    type = .x$type,
    date = as_datetime(.x$start_date_local),
    distance_km = .x$distance / 1000
  ))

print(activities_df)