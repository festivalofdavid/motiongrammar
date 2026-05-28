# segment name: auth ---

#' Refresh Strava API Tokens
#'
#' @param tokens A list containing client_id, client_secret, refresh_token, and access_token.
#' @param verbose Logical; if TRUE, prints status messages.
#' @return A list of updated tokens.
#' @keywords internal
get_valid_tokens <- function(tokens,
                            verbose = TRUE,
                            token_path = '~/strava_tokens.json'){
  now <- as.integer(Sys.time())

  if(!is.null(tokens$expires_at) && (tokens$expires_at - now) > 120){
    return(tokens)
  }

  if(verbose){
    message('Refreshing Strava tokens...')
  }

  req <- httr2::request('https://www.strava.com/oauth/token') |>
    httr2::req_body_form(client_id = tokens$client_id,
      client_secret = tokens$client_secret,
      grant_type = 'refresh_token',
      refresh_token = tokens$refresh_token)
  resp <- httr2::resp_body_json(.safe_perform(req, "Strava token refresh"))

  tokens$access_token <- resp$access_token
  tokens$refresh_token <- resp$refresh_token
  tokens$expires_at <- resp$expires_at

  path <- path.expand(token_path)
  if (!dir.exists(dirname(path))) {
    fallback <- file.path(tempdir(), basename(path))
    warning(
      "Token path directory not writable: ", dirname(path),
      "\nFalling back to: ", fallback,
      call. = FALSE
    )
    path <- fallback
  }
  tryCatch(
    writeLines(jsonlite::toJSON(tokens, auto_unbox = TRUE, pretty = TRUE), path),
    error = function(e) warning(
      "Could not write refreshed tokens to '", path, "': ", conditionMessage(e),
      call. = FALSE
    )
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
                               start_time_iso,
                               template = NULL){

  url <- paste0('https://www.strava.com/api/v3/activities/',
                activity_id,
                '/streams')

  t <- if (!is.null(template)) template else list(
    coord_system = "gps",
    stream_time = "time",
    stream_latlng = "latlng",
    stream_lat = NULL,
    stream_lng = NULL,
    stream_altitude = "altitude",
    stream_x = NULL,
    stream_y = NULL,
    stream_z = NULL,
    stream_velocity = NULL,
    stream_acceleration = NULL,
    stream_extra = NULL
  )

  keys_vec <- unique(c(
    t$stream_time,
    t$stream_latlng, t$stream_lat, t$stream_lng,
    t$stream_altitude,
    t$stream_x, t$stream_y, t$stream_z,
    t$stream_velocity, t$stream_acceleration,
    if (!is.null(t$stream_extra)) unname(t$stream_extra) else NULL
  ))
  keys_vec <- keys_vec[!is.na(keys_vec)]

  req <- httr2::request(url) |>
    httr2::req_auth_bearer_token(access_token) |>
    httr2::req_url_query(keys = paste(keys_vec, collapse = ","),
                         key_by_type = 'true')
  resp <- httr2::resp_body_json(.safe_perform(req, "Strava streams fetch"))

  start_unix <- as.numeric(lubridate::as_datetime(start_time_iso))
  n_rows <- if (length(resp) > 0) length(resp[[1]]$data) else 0L
  output <- list()

  for (key in names(resp)) {
    raw <- resp[[key]]$data
    if (key == t$stream_time) {
      output[["unix_time"]] <- start_unix + unlist(raw)
    } else if (!is.null(t$stream_latlng) && key == t$stream_latlng) {
      output[["lat"]] <- purrr::map_dbl(raw, 1)
      output[["lng"]] <- purrr::map_dbl(raw, 2)
    } else if (!is.null(t$stream_lat) && key == t$stream_lat) {
      output[["lat"]] <- unlist(raw)
    } else if (!is.null(t$stream_lng) && key == t$stream_lng) {
      output[["lng"]] <- unlist(raw)
    } else if (!is.null(t$stream_altitude) && key == t$stream_altitude) {
      output[["altitude"]] <- unlist(raw)
    } else if (!is.null(t$stream_x) && key == t$stream_x) {
      output[["x"]] <- unlist(raw)
    } else if (!is.null(t$stream_y) && key == t$stream_y) {
      output[["y"]] <- unlist(raw)
    } else if (!is.null(t$stream_z) && key == t$stream_z) {
      output[["z"]] <- unlist(raw)
    } else if (!is.null(t$stream_velocity) && key == t$stream_velocity) {
      output[["velocity"]] <- unlist(raw)
    } else if (!is.null(t$stream_acceleration) && key == t$stream_acceleration) {
      output[["acceleration"]] <- unlist(raw)
    } else if (!is.null(t$stream_extra) && key %in% unname(t$stream_extra)) {
      extra_nms <- names(t$stream_extra)
      idx <- which(unname(t$stream_extra) == key)[1]
      out_nm <- if (!is.null(extra_nms) && nchar(extra_nms[idx]) > 0) extra_nms[idx] else key
      output[[out_nm]] <- unlist(raw)
    }
  }

  coord_sys <- match.arg(t$coord_system %||% "gps", c("gps", "local"))
  core_cols <- if (coord_sys == "gps") c("unix_time", "lat", "lng", "altitude")
               else c("unix_time", "x", "y", "z")

  for (col in core_cols) {
    if (is.null(output[[col]])) output[[col]] <- rep(NA_real_, n_rows)
  }

  extra_cols <- setdiff(names(output), core_cols)

  if (!is.null(t$stream_extra) && length(t$stream_extra) > 0) {
    extra_nms_all <- names(t$stream_extra)
    out_names_extra <- vapply(seq_along(t$stream_extra), function(i) {
      if (!is.null(extra_nms_all) && nchar(extra_nms_all[i]) > 0) extra_nms_all[i] else t$stream_extra[[i]]
    }, character(1))
    .check_extra_collision(out_names_extra, "strava")
  }

  tibble::as_tibble(output[c(core_cols, extra_cols)])
}

#' Fetch and parse data from a generic REST JSON API
#' @keywords internal
fetch_generic_streams <- function(session, template) {

  t <- template

  # ── Build URL ─────────────────────────────────────────────────────────────
  url <- gsub("{session}", as.character(session), t$url, fixed = TRUE)

  # ── Resolve credentials ───────────────────────────────────────────────────
  resolve_env <- function(val, label) {
    if (!is.null(val) && nchar(val) > 0 && startsWith(val, "$")) {
      env_nm <- substring(val, 2)
      resolved <- Sys.getenv(env_nm, unset = NA_character_)
      if (is.na(resolved) || !nchar(resolved))
        stop("fetch_generic_streams(): environment variable '", env_nm,
             "' (", label, ") is not set.", call. = FALSE)
      return(resolved)
    }
    val
  }

  # ── Handle oauth2_refresh ─────────────────────────────────────────────────
  if (t$auth_type == "oauth2_refresh") {
    oauth_tokens <- get_valid_token_oauth2(
      token_url    = t$oauth2_token_url,
      token_path   = t$oauth2_token_path,
      client_id    = t$oauth2_client_id,
      client_secret = t$oauth2_client_secret
    )
    auth_val <- oauth_tokens$access_token
  } else {
    auth_val <- resolve_env(t$auth_value, "auth_value")
  }

  # ── Build request ─────────────────────────────────────────────────────────
  req <- httr2::request(url)

  if (t$auth_type %in% c("bearer", "oauth2_refresh")) {
    req <- httr2::req_auth_bearer_token(req, auth_val)
  } else if (t$auth_type == "api_key_header") {
    req <- do.call(httr2::req_headers, c(list(req), stats::setNames(list(auth_val), t$auth_header)))
  } else if (t$auth_type == "api_key_query") {
    req <- do.call(httr2::req_url_query, c(list(req), stats::setNames(list(auth_val), t$auth_param)))
  }

  if (!is.null(t$request_params) && length(t$request_params) > 0)
    req <- do.call(httr2::req_url_query, c(list(req), t$request_params))

  if (t$request_method == "POST") {
    if (!is.null(t$request_body))
      req <- httr2::req_body_json(req, t$request_body)
    req <- httr2::req_method(req, "POST")
  }

  resp <- httr2::resp_body_json(.safe_perform(req, "Generic API request"))

  # ── Navigate to data node ─────────────────────────────────────────────────
  data <- resp
  if (!is.null(t$data_path) && nchar(t$data_path) > 0) {
    for (key in strsplit(t$data_path, ".", fixed = TRUE)[[1]]) {
      if (is.null(data[[key]])) {
        available <- paste(names(data), collapse = ", ")
        stop(sprintf(
          "fetch_generic_streams(): data_path key '%s' not found in response.\n\nAvailable keys:\n  %s",
          key, available
        ), call. = FALSE)
      }
      data <- data[[key]]
    }
  }

  # ── Extract raw fields as character vectors ───────────────────────────────
  raw_fields <- switch(t$response_format,

    "records" = {
      if (!is.list(data) || length(data) == 0)
        stop("fetch_generic_streams(): response_format = 'records' expects a non-empty JSON array.\n\n",
             "Check:\n",
             "  - data_path points to the array (not a wrapper object)\n",
             "  - the session ID / endpoint returns data",
             call. = FALSE)
      all_field_names <- unique(unlist(lapply(data, names)))
      result <- lapply(all_field_names, function(nm) {
        vapply(data, function(rec) {
          val <- rec[[nm]]
          if (is.null(val)) NA_character_ else as.character(val[[1]])
        }, character(1))
      })
      stats::setNames(result, all_field_names)
    },

    "columnar" = {
      lapply(data, function(v) as.character(unlist(v)))
    },

    "streams" = {
      sdk <- t$stream_data_key %||% "data"
      lapply(data, function(stream) as.character(unlist(stream[[sdk]])))
    }
  )

  available_fields <- names(raw_fields)
  output <- list()

  # ── Time ──────────────────────────
  time_raw <- raw_fields[[t$stream_time]]
  if (is.null(time_raw)) {
    stop(sprintf(
      "fetch_generic_streams(): stream '%s' not found in API response.\n\nAvailable streams:\n  %s",
      t$stream_time, paste(available_fields, collapse = ", ")
    ), call. = FALSE)
  }

  output[["unix_time"]] <- switch(t$time_type,
    "unix"     = as.numeric(time_raw),
    "iso8601"  = as.numeric(lubridate::as_datetime(time_raw)),
    "relative" = {
      origin <- t$time_origin %||% 0
      origin + as.numeric(time_raw)
    },
    stop("fetch_generic_streams(): unsupported time_type '", t$time_type, "'", call. = FALSE)
  )

  # Canonical row count: anchored to the resolved time grid, not raw_fields[[1]].
  n_rows <- length(output[["unix_time"]])

  # Pad or truncate any stream whose length differs from the time grid.
  .align_to_grid <- function(vec, field_name) {
    n <- length(vec)
    if (n == n_rows) return(vec)
    warning(sprintf(
      "fetch_generic_streams(): stream '%s' has %d element(s) but the time grid has %d. %s.",
      field_name, n, n_rows,
      if (n < n_rows) "Padding tail with NA" else "Truncating to time grid length"
    ), call. = FALSE)
    if (n < n_rows) {
      c(vec, rep(if (is.numeric(vec)) NA_real_ else NA_character_, n_rows - n))
    } else {
      vec[seq_len(n_rows)]
    }
  }

  # ── Coordinates ────────
  # stream_latlng (combined pairs) only honoured in streams/columnar formats
  if (!is.null(t$stream_latlng) && t$response_format %in% c("streams", "columnar")) {
    pairs_src <- if (t$response_format == "streams") data[[t$stream_latlng]][[t$stream_data_key %||% "data"]]
                 else data[[t$stream_latlng]]
    if (!is.null(pairs_src)) {
      lat_raw <- purrr::map_dbl(pairs_src, 1)
      lng_raw <- purrr::map_dbl(pairs_src, 2)
      output[["lat"]] <- .align_to_grid(lat_raw, t$stream_latlng)
      output[["lng"]] <- .align_to_grid(lng_raw, t$stream_latlng)
    }
  } else {
    if (!is.null(t$stream_lat)) {
      if (t$stream_lat %in% available_fields)
        output[["lat"]] <- .align_to_grid(as.numeric(raw_fields[[t$stream_lat]]), t$stream_lat)
      else
        warning(sprintf(
          "stream_lat '%s' not found in API response. Available: %s",
          t$stream_lat, paste(available_fields, collapse = ", ")
        ), call. = FALSE)
    }
    if (!is.null(t$stream_lng)) {
      if (t$stream_lng %in% available_fields)
        output[["lng"]] <- .align_to_grid(as.numeric(raw_fields[[t$stream_lng]]), t$stream_lng)
      else
        warning(sprintf(
          "stream_lng '%s' not found in API response. Available: %s",
          t$stream_lng, paste(available_fields, collapse = ", ")
        ), call. = FALSE)
    }
  }

  .map_field <- function(key, out_nm) {
    if (!is.null(key)) {
      if (key %in% available_fields)
        output[[out_nm]] <<- .align_to_grid(as.numeric(raw_fields[[key]]), key)
      else
        warning(sprintf(
          "stream '%s' not found in API response. Available: %s",
          key, paste(available_fields, collapse = ", ")
        ), call. = FALSE)
    }
  }
  .map_field(t$stream_altitude,     "altitude")
  .map_field(t$stream_x,            "x")
  .map_field(t$stream_y,            "y")
  .map_field(t$stream_z,            "z")
  .map_field(t$stream_velocity,     "velocity")
  .map_field(t$stream_acceleration, "acceleration")

  # ── Extra streams ──────────────────────
  if (!is.null(t$stream_extra) && length(t$stream_extra) > 0) {
    extra_nms <- names(t$stream_extra)
    out_names_extra <- vapply(seq_along(t$stream_extra), function(i) {
      if (!is.null(extra_nms) && nchar(extra_nms[i]) > 0) extra_nms[i] else t$stream_extra[[i]]
    }, character(1))
    .check_extra_collision(out_names_extra, t$api %||% "api")

    for (i in seq_along(t$stream_extra)) {
      key <- t$stream_extra[[i]]
      out_nm <- out_names_extra[i]
      if (key %in% available_fields) {
        raw <- raw_fields[[key]]
        num <- suppressWarnings(as.numeric(raw))
        # Numeric only if every non-NA value converted cleanly; otherwise character.
        # Strict type prevents double↔character mismatch across batch API calls.
        typed <- if (all(!is.na(num) | is.na(raw))) num else as.character(raw)
        output[[out_nm]] <- .align_to_grid(typed, key)
      } else {
        warning(sprintf(
          "extra stream '%s' not found in API response. Available: %s",
          key, paste(available_fields, collapse = ", ")
        ), call. = FALSE)
      }
    }
  }

  # ── Fill missing core cols and assemble ──
  core_cols <- if (t$coord_system == "local") c("unix_time", "x", "y", "z")
               else c("unix_time", "lat", "lng", "altitude")

  for (col in core_cols) {
    if (is.null(output[[col]])) output[[col]] <- rep(NA_real_, n_rows)
  }

  extra_cols <- setdiff(names(output), core_cols)
  tibble::as_tibble(output[c(core_cols, extra_cols)])
}


# segment name: api_stream_template ---

#' Create an API Stream Template
#'
#' Defines a reusable connection, authentication, and field-mapping
#' specification for importing data from a REST JSON API. Pass the resulting
#' object to \code{\link{initiate}()} via the \code{template} argument, or
#' serialise it with \code{\link{save_api_stream_template}()}.
#'
#' \strong{Supported APIs:}
#' \itemize{
#'   \item \code{"strava"} — uses Strava's OAuth2 token-refresh flow and the
#'     \href{https://developers.strava.com/docs/reference/#api-Streams}{Streams
#'     endpoint}. Set \code{token_path} to your credentials JSON file.
#'   \item \code{"generic"} — any REST API returning JSON. Specify \code{url},
#'     \code{auth_type}, \code{auth_value}, and \code{response_format}.
#' }
#'
#' \strong{URL templates:} In \code{url}, the literal string \code{\{session\}}
#' is replaced at request time by the \code{session} argument passed to
#' \code{\link{initiate}()}. For example,
#' \code{"https://api.example.com/activities/\{session\}/data"}.
#'
#' \strong{Authentication (\code{auth_type}):}
#' \itemize{
#'   \item \code{"none"} — no credentials added.
#'   \item \code{"bearer"} — \code{Authorization: Bearer <auth_value>} header.
#'   \item \code{"api_key_header"} — \code{<auth_header>: <auth_value>} header
#'     (default header name: \code{"X-API-Key"}).
#'   \item \code{"api_key_query"} — appends \code{?<auth_param>=<auth_value>}
#'     to the URL (default param name: \code{"api_key"}).
#' }
#' If \code{auth_value} begins with \code{$} it is treated as an environment
#' variable name (e.g. \code{"$MY_API_KEY"} reads \code{Sys.getenv("MY_API_KEY")}),
#' which keeps credentials out of saved XML files.
#'
#' \strong{Response formats (\code{response_format}):}
#' \itemize{
#'   \item \code{"records"} — an array of row objects:
#'     \code{[\{"time": 0, "lat": 51.5, "lng": -0.1\}, ...]}.
#'   \item \code{"columnar"} — an object of parallel arrays:
#'     \code{\{"time": [...], "lat": [...], "lng": [...]\}}.
#'   \item \code{"streams"} — Strava-style: each key holds a sub-object whose
#'     \code{stream_data_key} entry (default \code{"data"}) is the array.
#' }
#' Use \code{data_path} (dot-separated) to navigate to the data node first,
#' e.g. \code{"result.streams"} for \code{resp$result$streams}.
#'
#' \strong{Combined lat/lng (\code{stream_latlng}):} honoured in
#' \code{"streams"} format where the API returns coordinate pairs as
#' \code{[[lat, lng], ...]}. For \code{"records"} and \code{"columnar"}
#' formats use separate \code{stream_lat}/\code{stream_lng}.
#'
#' \strong{Time formats (\code{time_type}):}
#' \itemize{
#'   \item \code{"unix"} — values are already Unix timestamps (seconds).
#'   \item \code{"iso8601"} — values are ISO 8601 strings.
#' }
#'
#' \code{stream_extra} accepts a named or unnamed character vector of
#' additional stream/field keys to carry through:
#' \itemize{
#'   \item \code{c("heartrate")} — import as-is; output column named \code{heartrate}.
#'   \item \code{c(heart_rate = "heartrate")} — import and rename to \code{heart_rate}.
#' }
#'
#' @param api Character; \code{"strava"} or \code{"generic"}.
#' @param coord_system Character; \code{"gps"} (lat/lng) or \code{"local"}
#'   (x/y). Defaults to \code{"gps"}.
#'
#' @param token_path Character; \emph{Strava only.} Path to the OAuth2
#'   credentials JSON file. Defaults to \code{"~/strava_tokens.json"}.
#'
#' @param url Character; \emph{Generic only.} Full endpoint URL. May contain
#'   \code{\{session\}} as a placeholder for the session/activity identifier
#'   supplied to \code{\link{initiate}()}. Required when
#'   \code{api = "generic"}.
#' @param request_method Character; HTTP method: \code{"GET"} (default) or
#'   \code{"POST"}.
#' @param request_params Named list; additional query parameters appended to
#'   the URL for every request.
#' @param request_body Named list; JSON body for \code{POST} requests.
#' @param auth_type Character; authentication method. One of \code{"none"},
#'   \code{"bearer"}, \code{"api_key_header"}, \code{"api_key_query"}.
#'   Defaults to \code{"none"}.
#' @param auth_value Character; token or API key. If it starts with \code{$},
#'   interpreted as an environment variable name.
#' @param auth_header Character; header name used when
#'   \code{auth_type = "api_key_header"}. Defaults to \code{"X-API-Key"}.
#' @param auth_param Character; query parameter name used when
#'   \code{auth_type = "api_key_query"}. Defaults to \code{"api_key"}.
#' @param response_format Character; JSON structure of the response:
#'   \code{"records"}, \code{"columnar"}, or \code{"streams"}. Defaults to
#'   \code{"records"}.
#' @param data_path Character; dot-separated path to the data node within the
#'   response (e.g. \code{"data"} or \code{"result.streams"}). Leave
#'   \code{NULL} to use the top-level response.
#' @param stream_data_key Character; for \code{response_format = "streams"},
#'   the key within each stream object that holds the data array. Defaults to
#'   \code{"data"}.
#' @param time_type Character; how to interpret the time stream: \code{"unix"}
#'   (already Unix seconds) or \code{"iso8601"} (ISO 8601 strings). Strava
#'   uses its own internal offset mechanism and ignores this parameter.
#'
#' @param stream_time Character; stream/field key for timestamps. Required.
#'   Defaults to \code{"time"} for Strava.
#' @param stream_latlng Character; stream key for combined lat/lng pairs
#'   (\code{"streams"} format only). Defaults to \code{"latlng"} for Strava
#'   GPS. Use \code{stream_lat}/\code{stream_lng} for separate-field APIs.
#' @param stream_lat Character; field key for latitude. Optional.
#' @param stream_lng Character; field key for longitude. Optional.
#' @param stream_altitude Character; field key for altitude. Optional.
#'   Defaults to \code{"altitude"} for Strava GPS.
#' @param stream_x Character; field key for x-coordinate (local). Required for
#'   local coordinate systems.
#' @param stream_y Character; field key for y-coordinate (local). Required for
#'   local coordinate systems.
#' @param stream_z Character; field key for z-coordinate (local). Optional.
#' @param stream_velocity Character; field key for a pre-computed velocity.
#'   Imported as \code{velocity}. Optional.
#' @param stream_acceleration Character; field key for a pre-computed
#'   acceleration. Imported as \code{acceleration}. Optional.
#' @param stream_extra Named or unnamed character vector of additional
#'   stream/field keys to carry through. See Details.
#'
#' @return An object of class \code{api_stream_template}.
#' @seealso \code{\link{save_api_stream_template}},
#'   \code{\link{load_api_stream_template}}, \code{\link{initiate}}
#' @export
api_stream_template <- function(
    api = c("strava", "generic"),
    coord_system = c("gps", "local"),
    # Strava-specific
    token_path = "~/strava_tokens.json",
    # Generic REST
    url = NULL,
    request_method = c("GET", "POST"),
    request_params = NULL,
    request_body = NULL,
    auth_type = c("none", "bearer", "api_key_header", "api_key_query", "oauth2_refresh"),
    auth_value = NULL,
    auth_header = "X-API-Key",
    auth_param = "api_key",
    # OAuth2 refresh fields (used when auth_type = "oauth2_refresh")
    oauth2_token_url = NULL,
    oauth2_client_id = NULL,
    oauth2_client_secret = NULL,
    oauth2_token_path = NULL,
    response_format = c("records", "columnar", "streams"),
    data_path = NULL,
    stream_data_key = "data",
    time_type = c("unix", "iso8601", "relative"),
    time_origin = NULL,
    # Stream/field mapping (all APIs)
    stream_time = NULL,
    stream_latlng = NULL,
    stream_lat = NULL,
    stream_lng = NULL,
    stream_altitude = NULL,
    stream_x = NULL,
    stream_y = NULL,
    stream_z = NULL,
    stream_velocity = NULL,
    stream_acceleration = NULL,
    stream_extra = NULL
) {
  api <- match.arg(api)
  coord_system <- match.arg(coord_system)
  request_method <- match.arg(request_method)
  auth_type <- match.arg(auth_type)
  response_format <- match.arg(response_format)
  time_type <- match.arg(time_type)

  # ── Strava defaults ──────────────────────────────────────────────────────────
  if (api == "strava") {
    if (is.null(stream_time)) stream_time <- "time"
    if (coord_system == "gps") {
      if (is.null(stream_latlng) && is.null(stream_lat) && is.null(stream_lng))
        stream_latlng <- "latlng"
      if (is.null(stream_altitude))
        stream_altitude <- "altitude"
    }
  }

  # ── Validation ───────────────────────────────────────────────────────────────
  if (is.null(stream_time) || !nchar(stream_time))
    stop("api_stream_template(): stream_time is required")

  if (api == "generic" && (is.null(url) || !nchar(url)))
    stop("api_stream_template(): url is required when api = 'generic'")

  non_oauth2_auth <- c("bearer", "api_key_header", "api_key_query")
  if (auth_type %in% non_oauth2_auth && (is.null(auth_value) || !nchar(auth_value)))
    stop("api_stream_template(): auth_value is required when auth_type = '", auth_type, "'")

  if (auth_type == "oauth2_refresh") {
    if (is.null(oauth2_token_url) || !nchar(oauth2_token_url))
      stop("api_stream_template(): oauth2_token_url is required when auth_type = 'oauth2_refresh'")
    if (is.null(oauth2_token_path) || !nchar(oauth2_token_path))
      stop("api_stream_template(): oauth2_token_path is required when auth_type = 'oauth2_refresh'")
  }

  if (time_type == "relative" && !is.null(time_origin) && !is.numeric(time_origin))
    stop("api_stream_template(): time_origin must be a numeric Unix timestamp")

  if (coord_system == "gps") {
    if (is.null(stream_latlng) && (is.null(stream_lat) || is.null(stream_lng)))
      stop("api_stream_template(): coord_system = 'gps' requires stream_latlng ",
           "or both stream_lat and stream_lng")
  } else {
    if (is.null(stream_x))
      stop("api_stream_template(): coord_system = 'local' requires stream_x")
    if (is.null(stream_y))
      stop("api_stream_template(): coord_system = 'local' requires stream_y")
  }

  structure(
    list(
      api = api,
      coord_system = coord_system,
      # Strava
      token_path = as.character(token_path),
      # Generic REST
      url = if (!is.null(url)) as.character(url) else NULL,
      request_method = request_method,
      request_params = request_params,
      request_body = request_body,
      auth_type = auth_type,
      auth_value = if (!is.null(auth_value)) as.character(auth_value) else NULL,
      auth_header = as.character(auth_header),
      auth_param = as.character(auth_param),
      # OAuth2 refresh
      oauth2_token_url    = if (!is.null(oauth2_token_url))    as.character(oauth2_token_url)    else NULL,
      oauth2_client_id    = if (!is.null(oauth2_client_id))    as.character(oauth2_client_id)    else NULL,
      oauth2_client_secret = if (!is.null(oauth2_client_secret)) as.character(oauth2_client_secret) else NULL,
      oauth2_token_path   = if (!is.null(oauth2_token_path))   as.character(oauth2_token_path)   else NULL,
      response_format = response_format,
      data_path = if (!is.null(data_path)) as.character(data_path) else NULL,
      stream_data_key = as.character(stream_data_key),
      time_type = time_type,
      time_origin = if (!is.null(time_origin)) as.numeric(time_origin) else NULL,
      # Field mapping
      stream_time = as.character(stream_time),
      stream_latlng = if (!is.null(stream_latlng))       as.character(stream_latlng)       else NULL,
      stream_lat = if (!is.null(stream_lat))          as.character(stream_lat)          else NULL,
      stream_lng = if (!is.null(stream_lng))          as.character(stream_lng)          else NULL,
      stream_altitude = if (!is.null(stream_altitude))     as.character(stream_altitude)     else NULL,
      stream_x = if (!is.null(stream_x))            as.character(stream_x)            else NULL,
      stream_y = if (!is.null(stream_y))            as.character(stream_y)            else NULL,
      stream_z = if (!is.null(stream_z))            as.character(stream_z)            else NULL,
      stream_velocity = if (!is.null(stream_velocity))     as.character(stream_velocity)     else NULL,
      stream_acceleration = if (!is.null(stream_acceleration)) as.character(stream_acceleration) else NULL,
      stream_extra = stream_extra
    ),
    class = "api_stream_template"
  )
}

#' @method print api_stream_template
#' @export
print.api_stream_template <- function(x, ...) {
  cat(sprintf("API Stream Template [%s / %s]\n", toupper(x$api), x$coord_system))

  if (x$api == "strava") {
    cat(sprintf("  Token path:  %s\n", x$token_path))
  } else {
    cat(sprintf("  URL:         %s\n", x$url))
    cat(sprintf("  Method:      %s\n", x$request_method))
    if (x$auth_type == "oauth2_refresh") {
      cid_disp <- if (!is.null(x$oauth2_client_id) && startsWith(x$oauth2_client_id, "$"))
        x$oauth2_client_id else "<set>"
      cat(sprintf("  Auth:        oauth2_refresh  (token: %s  client_id: %s)\n",
                  x$oauth2_token_path %||% "?", cid_disp))
      cat(sprintf("  Token URL:   %s\n", x$oauth2_token_url %||% "?"))
    } else if (x$auth_type != "none") {
      disp_val <- if (!is.null(x$auth_value) && startsWith(x$auth_value, "$"))
        x$auth_value
      else
        "<hidden>"
      cat(sprintf("  Auth:        %s  (%s)\n", x$auth_type, disp_val))
    }
    cat(sprintf("  Response:    %s", x$response_format))
    if (!is.null(x$data_path)) cat(sprintf("  (path: %s)", x$data_path))
    time_str <- x$time_type
    if (x$time_type == "relative" && !is.null(x$time_origin))
      time_str <- sprintf("relative (origin: %.0f)", x$time_origin)
    cat(sprintf("  |  time_type: %s\n", time_str))
  }

  cat("  Stream mapping:\n")
  cat(sprintf("    unix_time <- %s\n", x$stream_time))
  if (!is.null(x$stream_latlng))
    cat(sprintf("    lat, lng <- %s (combined pairs)\n", x$stream_latlng))
  if (!is.null(x$stream_lat))  cat(sprintf("    lat <- %s\n", x$stream_lat))
  if (!is.null(x$stream_lng))  cat(sprintf("    lng <- %s\n", x$stream_lng))
  if (!is.null(x$stream_altitude))
    cat(sprintf("    altitude <- %s\n", x$stream_altitude))
  if (!is.null(x$stream_x))    cat(sprintf("    x <- %s\n", x$stream_x))
  if (!is.null(x$stream_y))    cat(sprintf("    y <- %s\n", x$stream_y))
  if (!is.null(x$stream_z))    cat(sprintf("    z <- %s\n", x$stream_z))
  if (!is.null(x$stream_velocity))
    cat(sprintf("    velocity <- %s\n", x$stream_velocity))
  if (!is.null(x$stream_acceleration))
    cat(sprintf("    acceleration <- %s\n", x$stream_acceleration))
  if (!is.null(x$stream_extra) && length(x$stream_extra) > 0) {
    nms <- names(x$stream_extra)
    parts <- vapply(seq_along(x$stream_extra), function(i) {
      key <- x$stream_extra[[i]]
      out_nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else key
      if (out_nm == key) key else sprintf("%s <- %s", out_nm, key)
    }, character(1))
    cat(sprintf("  Extra streams: %s\n", paste(parts, collapse = ", ")))
  }
  invisible(x)
}

#' Save an API Stream Template to an XML File
#'
#' Serialises an \code{api_stream_template} object to a portable XML file that
#' can be shared across scripts, checked into version control, or loaded later
#' with \code{\link{load_api_stream_template}()}.
#'
#' @param template An \code{api_stream_template} object.
#' @param path Character; file path for the output \code{.xml} file.
#'
#' @return Invisibly returns \code{template}.
#' @seealso \code{\link{api_stream_template}},
#'   \code{\link{load_api_stream_template}}
#' @export
save_api_stream_template <- function(template, path) {
  if (!inherits(template, "api_stream_template"))
    stop("save_api_stream_template(): template must be an api_stream_template object")

  add_tag <- function(name, value, indent = "  ") {
    if (is.null(value) || (length(value) == 1 && is.na(value)))
      return(character(0))
    sprintf("%s<%s>%s</%s>", indent, name, .xml_escape(as.character(value)), name)
  }

  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<api_stream_template>",
    add_tag("api",          template$api),
    add_tag("coord_system", template$coord_system),
    add_tag("token_path",   template$token_path),
    "  <request>",
    add_tag("url",            template$url,            "    "),
    add_tag("request_method", template$request_method, "    "),
    add_tag("data_path",      template$data_path,      "    "),
    "  </request>",
    "  <auth>",
    add_tag("auth_type",   template$auth_type,   "    "),
    add_tag("auth_value",  template$auth_value,  "    "),
    add_tag("auth_header", template$auth_header, "    "),
    add_tag("auth_param",  template$auth_param,  "    "),
    add_tag("oauth2_token_url",    template$oauth2_token_url,    "    "),
    add_tag("oauth2_client_id",    template$oauth2_client_id,    "    "),
    add_tag("oauth2_client_secret", template$oauth2_client_secret, "    "),
    add_tag("oauth2_token_path",   template$oauth2_token_path,   "    "),
    "  </auth>",
    "  <response>",
    add_tag("response_format", template$response_format, "    "),
    add_tag("stream_data_key", template$stream_data_key, "    "),
    add_tag("time_type",       template$time_type,       "    "),
    add_tag("time_origin",     template$time_origin,     "    "),
    "  </response>",
    "  <streams>",
    add_tag("stream_time",         template$stream_time,         "    "),
    add_tag("stream_latlng",       template$stream_latlng,       "    "),
    add_tag("stream_lat",          template$stream_lat,          "    "),
    add_tag("stream_lng",          template$stream_lng,          "    "),
    add_tag("stream_altitude",     template$stream_altitude,     "    "),
    add_tag("stream_x",            template$stream_x,            "    "),
    add_tag("stream_y",            template$stream_y,            "    "),
    add_tag("stream_z",            template$stream_z,            "    "),
    add_tag("stream_velocity",     template$stream_velocity,     "    "),
    add_tag("stream_acceleration", template$stream_acceleration, "    "),
    "  </streams>"
  )

  if (!is.null(template$stream_extra) && length(template$stream_extra) > 0) {
    nms <- names(template$stream_extra)
    extra_tags <- vapply(seq_along(template$stream_extra), function(i) {
      ref <- template$stream_extra[[i]]
      nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL
      as_attr <- if (!is.null(nm)) sprintf(' as="%s"', .xml_escape(nm)) else ""
      sprintf('    <stream ref="%s"%s/>', .xml_escape(ref), as_attr)
    }, character(1))
    lines <- c(lines, "  <extra_streams>", extra_tags, "  </extra_streams>")
  }

  lines <- c(lines, "</api_stream_template>")
  writeLines(lines, path)
  invisible(template)
}

#' Load an API Stream Template from an XML File
#'
#' Reads an XML file written by \code{\link{save_api_stream_template}()} and
#' returns an \code{api_stream_template} object ready to pass to
#' \code{\link{initiate}()}.
#'
#' @param path Character; path to the XML file.
#'
#' @return An \code{api_stream_template} object.
#' @seealso \code{\link{api_stream_template}},
#'   \code{\link{save_api_stream_template}}
#' @export
load_api_stream_template <- function(path) {
  if (!file.exists(path))
    stop("load_api_stream_template(): file not found: ", path)

  content <- paste(readLines(path, warn = FALSE), collapse = "\n")

  get_tag <- function(tag, text) {
    pattern <- sprintf("<%s>([^<]*)</%s>", tag, tag)
    m <- regmatches(text, regexpr(pattern, text, perl = TRUE))
    if (length(m) == 0 || identical(m, character(0))) return(NULL)
    val <- sub(sprintf("^<%s>", tag), "", sub(sprintf("</%s>$", tag), "", m))
    val <- .xml_unescape(val)
    if (nchar(val) == 0) NULL else val
  }

  extract_section <- function(tag, text) {
    pat <- sprintf("(?s)<%s>(.*?)</%s>", tag, tag)
    m <- regexpr(pat, text, perl = TRUE)
    if (m > 0) regmatches(text, m) else text
  }

  request_section <- extract_section("request",  content)
  auth_section <- extract_section("auth",     content)
  response_section <- extract_section("response", content)
  streams_section <- extract_section("streams",  content)

  api <- get_tag("api",          content) %||% "strava"
  coord_system <- get_tag("coord_system", content) %||% "gps"
  token_path <- get_tag("token_path",   content) %||% "~/strava_tokens.json"

  url <- get_tag("url",            request_section)
  request_method <- get_tag("request_method", request_section) %||% "GET"
  data_path <- get_tag("data_path",      request_section)

  auth_type <- get_tag("auth_type",   auth_section) %||% "none"
  auth_value <- get_tag("auth_value",  auth_section)
  auth_header <- get_tag("auth_header", auth_section) %||% "X-API-Key"
  auth_param <- get_tag("auth_param",  auth_section) %||% "api_key"
  oauth2_token_url    <- get_tag("oauth2_token_url",    auth_section)
  oauth2_client_id    <- get_tag("oauth2_client_id",    auth_section)
  oauth2_client_secret <- get_tag("oauth2_client_secret", auth_section)
  oauth2_token_path   <- get_tag("oauth2_token_path",   auth_section)

  response_format <- get_tag("response_format", response_section) %||% "records"
  stream_data_key <- get_tag("stream_data_key", response_section) %||% "data"
  time_type <- get_tag("time_type",       response_section) %||% "unix"
  time_origin_raw <- get_tag("time_origin", response_section)
  time_origin <- if (!is.null(time_origin_raw)) as.numeric(time_origin_raw) else NULL

  stream_time <- get_tag("stream_time",         streams_section)
  stream_latlng <- get_tag("stream_latlng",       streams_section)
  stream_lat <- get_tag("stream_lat",          streams_section)
  stream_lng <- get_tag("stream_lng",          streams_section)
  stream_altitude <- get_tag("stream_altitude",     streams_section)
  stream_x <- get_tag("stream_x",            streams_section)
  stream_y <- get_tag("stream_y",            streams_section)
  stream_z <- get_tag("stream_z",            streams_section)
  stream_velocity <- get_tag("stream_velocity",     streams_section)
  stream_acceleration <- get_tag("stream_acceleration", streams_section)

  if (is.null(stream_time))
    stop("load_api_stream_template(): <stream_time> not found in <streams>")

  stream_extra <- NULL
  extra_m <- regexpr("(?s)<extra_streams>(.*?)</extra_streams>", content, perl = TRUE)
  if (extra_m > 0) {
    extra_section <- regmatches(content, extra_m)
    ref_matches <- gregexpr('<stream ref="[^"]*"[^/]*/>', extra_section, perl = TRUE)
    ref_vals <- regmatches(extra_section, ref_matches)[[1]]

    if (length(ref_vals) > 0) {
      refs <- .xml_unescape(gsub('^.*<stream ref="([^"]*)".*$', "\\1", ref_vals))
      as_vals <- vapply(ref_vals, function(tag) {
        if (!grepl('as="', tag, fixed = TRUE)) return(NA_character_)
        .xml_unescape(gsub('^.*as="([^"]*)".*$', "\\1", tag))
      }, character(1))
      nms <- ifelse(is.na(as_vals), "", as_vals)
      stream_extra <- stats::setNames(refs, nms)
    }
  }

  structure(
    list(
      api = api,
      coord_system = coord_system,
      token_path = token_path,
      url = url,
      request_method = request_method,
      request_params = NULL,
      request_body = NULL,
      auth_type = auth_type,
      auth_value = auth_value,
      auth_header = auth_header,
      auth_param = auth_param,
      oauth2_token_url    = oauth2_token_url,
      oauth2_client_id    = oauth2_client_id,
      oauth2_client_secret = oauth2_client_secret,
      oauth2_token_path   = oauth2_token_path,
      response_format = response_format,
      data_path = data_path,
      stream_data_key = stream_data_key,
      time_type = time_type,
      time_origin = time_origin,
      stream_time = stream_time,
      stream_latlng = stream_latlng,
      stream_lat = stream_lat,
      stream_lng = stream_lng,
      stream_altitude = stream_altitude,
      stream_x = stream_x,
      stream_y = stream_y,
      stream_z = stream_z,
      stream_velocity = stream_velocity,
      stream_acceleration = stream_acceleration,
      stream_extra = stream_extra
    ),
    class = "api_stream_template"
  )
}
