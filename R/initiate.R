# segment name: helpers ---

# Null coalescing — returns b when a is NULL
`%||%` <- function(a, b){
  if(is.null(a)) b else a
}

#' Pipeline-reserved column names produced by derivate() and designate()
#' @keywords internal
.mg_pipeline_reserved <- c(
  'distance', 'velocity', 'acceleration', 'heading',
  'angular_velocity', 'jerk', 'lateral_g', 'designation'
)

#' Warn on reserved column name collision in user-supplied extra columns
#' @keywords internal
.check_extra_collision <- function(output_names, api_label = "api") {
  hits <- intersect(output_names, .mg_pipeline_reserved)
  for (nm in hits) {
    warning(
      "External field '", nm, "' conflicts with a reserved motiongrammar pipeline output.\n",
      "  derivate() / designate() will overwrite it. ",
      "Consider renaming to '", api_label, "_", nm,
      "' in your template's stream_extra / col_extra.",
      call. = FALSE
    )
  }
}

#' httr2 req_perform wrapper with human-readable HTTP error messages
#' @keywords internal
.safe_perform <- function(req, context = "API request") {
  # Specific HTTP status handlers are listed first; no generic error handler
  # to avoid double-wrapping (R tryCatch re-matches inner stop() calls).
  tryCatch(
    httr2::req_perform(req),
    httr2_http_401 = function(e) stop(
      context, " failed: authentication error (401 Unauthorized).\n\n",
      "Check:\n",
      "  - token validity\n",
      "  - refresh token\n",
      "  - client credentials\n",
      "  - internet connection",
      call. = FALSE
    ),
    httr2_http_403 = function(e) stop(
      context, " failed: access forbidden (403 Forbidden).\n\n",
      "Check:\n",
      "  - token scopes / permissions\n",
      "  - account access level",
      call. = FALSE
    ),
    httr2_http_404 = function(e) stop(
      context, " failed: resource not found (404 Not Found).\n\n",
      "Check:\n",
      "  - session / activity ID\n",
      "  - API endpoint URL",
      call. = FALSE
    ),
    httr2_http_429 = function(e) stop(
      context, " failed: rate limited (429 Too Many Requests).\n\n",
      "Wait before retrying or check your API quota.",
      call. = FALSE
    ),
    httr2_http = function(e) stop(
      context, " failed:\n", conditionMessage(e),
      call. = FALSE
    )
  )
}

#' Generic OAuth2 token refresh (any provider)
#'
#' Loads tokens from a JSON file, refreshes if expired, and saves back.
#' Token file must contain \code{refresh_token}; may contain
#' \code{client_id}, \code{client_secret}, and \code{expires_at}.
#'
#' @param token_url Character; the OAuth2 token endpoint URL.
#' @param token_path Character; path to the JSON credentials file.
#' @param client_id Character or NULL; overrides the value in the JSON.
#'   A leading \code{$} reads from an environment variable.
#' @param client_secret Character or NULL; same env-var semantics.
#' @param verbose Logical; if TRUE, prints refresh status.
#' @return A list containing at least \code{access_token}.
#' @keywords internal
get_valid_token_oauth2 <- function(token_url,
                                   token_path,
                                   client_id = NULL,
                                   client_secret = NULL,
                                   verbose = TRUE) {

  resolve_secret <- function(val) {
    if (!is.null(val) && nchar(val) > 0 && startsWith(val, "$")) {
      env_nm <- substring(val, 2)
      resolved <- Sys.getenv(env_nm, unset = NA_character_)
      if (is.na(resolved) || !nchar(resolved))
        stop("oauth2: environment variable '", env_nm, "' is not set.", call. = FALSE)
      return(resolved)
    }
    val
  }

  path_exp <- path.expand(token_path)
  if (!file.exists(path_exp))
    stop(
      "oauth2: token file not found: ", path_exp, "\n\n",
      "Obtain credentials from your API provider and save them as JSON:\n",
      "  {\"access_token\": \"...\", \"refresh_token\": \"...\", \"expires_at\": ...,\n",
      "   \"client_id\": \"...\", \"client_secret\": \"...\"}",
      call. = FALSE
    )

  tokens <- jsonlite::fromJSON(path_exp)
  now <- as.integer(Sys.time())

  if (!is.null(tokens$expires_at) && (tokens$expires_at - now) > 120)
    return(tokens)

  if (verbose) message("Refreshing OAuth2 tokens...")

  cid  <- resolve_secret(client_id  %||% tokens$client_id)
  csec <- resolve_secret(client_secret %||% tokens$client_secret)

  if (is.null(cid)  || !nchar(cid))
    stop("oauth2: client_id not found. ",
         "Provide it via oauth2_client_id in the template or add 'client_id' to the token JSON.",
         call. = FALSE)
  if (is.null(csec) || !nchar(csec))
    stop("oauth2: client_secret not found. ",
         "Provide it via oauth2_client_secret in the template or add 'client_secret' to the token JSON.",
         call. = FALSE)
  if (is.null(tokens$refresh_token) || !nchar(tokens$refresh_token))
    stop("oauth2: refresh_token not found in token file: ", path_exp, call. = FALSE)

  resp <- tryCatch(
    httr2::request(token_url) |>
      httr2::req_body_form(
        client_id     = cid,
        client_secret = csec,
        grant_type    = "refresh_token",
        refresh_token = tokens$refresh_token
      ) |>
      httr2::req_perform() |>
      httr2::resp_body_json(),
    httr2_http_400 = function(e)
      stop("oauth2 token refresh failed (400 Bad Request).\n\n",
           "Check:\n",
           "  - refresh_token is still valid (may have expired or been revoked)\n",
           "  - client_id and client_secret are correct",
           call. = FALSE),
    httr2_http_401 = function(e)
      stop("oauth2 token refresh failed (401 Unauthorized).\n\n",
           "Check:\n",
           "  - client_id and client_secret are correct\n",
           "  - token endpoint: ", token_url,
           call. = FALSE),
    error = function(e) stop("oauth2 token refresh failed: ", conditionMessage(e), call. = FALSE)
  )

  tokens$access_token <- resp$access_token
  if (!is.null(resp$refresh_token)) tokens$refresh_token <- resp$refresh_token
  tokens$expires_at <- if (!is.null(resp$expires_in))
    now + as.integer(resp$expires_in)
  else
    resp$expires_at %||% tokens$expires_at

  writeLines(jsonlite::toJSON(tokens, auto_unbox = TRUE, pretty = TRUE), path_exp)
  tokens
}

#' Validate a motion_trace Object
#'
#' Called at the entry of every major pipeline function. Checks that the input
#' is a \code{motion_trace}, has \code{unix_time}, and optionally that specific
#' columns are present.
#'
#' @param .data Object to validate.
#' @param call Character; calling function name, used in error messages.
#' @param requires Character vector of additional required column names.
#' @keywords internal
validate_motion_trace <- function(.data, call = 'function', requires = character(0)) {

  if (!inherits(.data, 'motion_trace')) {
    stop(
      call, '(): input must be a motion_trace object created by initiate(). ',
      'If a dplyr verb stripped the class, wrap the operation in elaborate() ',
      'or ensure dplyr >= 1.1 and vctrs >= 0.6 are installed.'
    )
  }

  required <- c('unix_time', requires)
  missing <- setdiff(required, names(.data))
  if (length(missing) > 0) {
    stop(
      call, '(): required column(s) missing: ',
      paste(missing, collapse = ', '),
      '. Ensure all prior pipeline steps have been completed.'
    )
  }

  if (is.null(attr(.data, 'metadata'))) {
    warning(
      call, '(): metadata attribute is missing. ',
      'Quality and reproducibility tracking will be incomplete.'
    )
  }

  invisible(.data)
}

#' vctrs restore method for motion_trace
#'
#' Ensures that dplyr verbs (filter, slice, arrange, rename, etc.) preserve
#' the motion_trace class and its metadata and quality attributes.
#'
#' @param x The result of a dplyr operation.
#' @param to The original motion_trace object.
#' @keywords internal
#' @importFrom vctrs vec_restore
#' @export
vec_restore.motion_trace <- function(x, to, ...) {
  attr(x, 'metadata') <- attr(to, 'metadata')
  attr(x, 'quality') <- attr(to, 'quality')
  class(x) <- class(to)
  x
}

#' Create Metadata
#' @keywords internal
create_metadata <- function(session,
                           source,
                           trace,
                           device_info = NULL,
                           coord_system = 'gps'){

  session_name <- if(file.exists(session)){
    basename(session)
  } else {
    as.character(session)
  }

  date_from_name <- tryCatch({
    date_patterns <- c(
      "\\d{4}-\\d{2}-\\d{2}",
      "\\d{8}",
      "\\d{2}-\\d{2}-\\d{4}"
    )

    for(pattern in date_patterns){
      match <- stringr::str_extract(session_name, pattern)
      if(!is.na(match)){
        parsed <- lubridate::parse_date_time(match, orders = c('ymd', 'dmy'))
        if(!is.na(parsed)) return(as.Date(parsed))
      }
    }
    NA
  }, error = function(e) NA)

  time_diffs <- diff(trace$unix_time)
  time_diffs <- time_diffs[!is.na(time_diffs) & time_diffs > 0]

  if(length(time_diffs) > 0){
    median_dt <- median(time_diffs)
    native_hz <- round(1 / median_dt, 2)
  } else {
    median_dt <- NA
    native_hz <- NA
  }

  time_range <- range(trace$unix_time, na.rm = TRUE)
  duration_sec <- if(!any(is.na(time_range))) diff(time_range) else NA

  pkg_version <- .mg_pkg_version()

  metadata <- list(
    name = session_name,
    source = source,
    session_id = NA_character_,

    player_id = NA_character_,
    player_name = NA_character_,
    team = NA_character_,

    device_type = if(!is.null(device_info$type)) device_info$type else NA_character_,
    device_id = if(!is.null(device_info$id)) device_info$id else NA_character_,
    device_manufacturer = if(!is.null(device_info$manufacturer)) device_info$manufacturer else NA_character_,
    firmware_version = if(!is.null(device_info$firmware)) device_info$firmware else NA_character_,

    sport = NA_character_,
    session_type = NA_character_,
    session_start = date_from_name,
    session_duration_sec = as.numeric(duration_sec),

    coordinate_system = coord_system,
    native_hz = native_hz,
    median_dt = median_dt,

    column_mapping = NULL,

    created_timestamp = Sys.time(),
    created_by = 'motiongrammar',
    package_version = pkg_version
  )

  return(metadata)
}

#' Update Metadata
#' @export
set_metadata <- function(.data, ...){

  if(!inherits(.data, 'motion_trace')){
    stop("Input must be a motion_trace object")
  }

  meta <- attr(.data, 'metadata')

  if(is.null(meta)){
    warning("No metadata found. Creating new metadata structure.")
    meta <- list()
  }

  new_meta <- list(...)

  for(key in names(new_meta)){
    meta[[key]] <- new_meta[[key]]
  }

  attr(.data, 'metadata') <- meta
  return(.data)
}

#' Convert an Existing Data Frame to a motion_trace
#' @description Wraps an in-memory data frame or tibble in the
#'   \code{motion_trace} class without reading from disk. Use this when
#'   tracking data arrives via a database query, a simulation, a model
#'   output, or any other R object that was not loaded through
#'   \code{initiate()}.
#' @param .data A data frame or tibble. Must not already be a
#'   \code{motion_trace}.
#' @param coord_system Character; \code{"gps"} (expects \code{lat}/\code{lng}
#'   columns) or \code{"local"} (expects \code{x}/\code{y} columns).
#' @param col_time Name of the unix timestamp column. \code{NULL} (default)
#'   auto-detects from common names: \code{unix_time}, \code{time},
#'   \code{timestamp}, \code{t}, \code{unix}, \code{epoch}.
#' @param col_lat,col_lng GPS coordinate column names. \code{NULL}
#'   auto-detects from \code{lat}/\code{latitude} and
#'   \code{lng}/\code{lon}/\code{longitude}. Ignored when
#'   \code{coord_system = "local"}.
#' @param col_x,col_y,col_z Local coordinate column names. \code{NULL}
#'   auto-detects columns named \code{x}, \code{y}, \code{z}.
#'   \code{col_z} is optional. Ignored when \code{coord_system = "gps"}.
#' @param name A string stored as the session name in metadata. Defaults to
#'   \code{"as_motion_trace"}.
#' @param ... Additional metadata fields forwarded to
#'   \code{\link{set_metadata}} (e.g. \code{player_name = "Alice"},
#'   \code{sport = "Football"}).
#' @return A \code{motion_trace} object with \code{metadata} and
#'   \code{quality} attributes populated, ready for any downstream
#'   pipeline step.
#' @export
as_motion_trace <- function(
  .data,
  coord_system = c('gps', 'local'),
  col_time     = NULL,
  col_lat      = NULL,
  col_lng      = NULL,
  col_x        = NULL,
  col_y        = NULL,
  col_z        = NULL,
  name         = 'as_motion_trace',
  ...
) {
  if (!is.data.frame(.data)) {
    stop("as_motion_trace(): .data must be a data frame or tibble.")
  }
  if (inherits(.data, 'motion_trace')) {
    stop(
      "as_motion_trace(): .data is already a motion_trace. ",
      "Use set_metadata() to update its fields."
    )
  }

  coord_system <- match.arg(coord_system)
  out <- tibble::as_tibble(.data)

  # ── Helper: resolve a column name from an explicit value or a priority list ──
  .resolve_col <- function(explicit, candidates, role, required = TRUE) {
    nms_lower <- tolower(names(out))
    if (!is.null(explicit)) {
      if (!explicit %in% names(out))
        stop("as_motion_trace(): col for '", role, "' ('", explicit,
             "') not found in .data.")
      return(explicit)
    }
    for (cand in candidates) {
      hit <- which(nms_lower == cand)
      if (length(hit) > 0) return(names(out)[hit[1]])
    }
    if (required)
      stop(
        "as_motion_trace(): could not detect a '", role, "' column. ",
        "Expected one of: ", paste(candidates, collapse = ', '),
        ". Provide col_", role, " = 'your_column_name'."
      )
    NULL
  }

  # ── Resolve unix_time ───────────────────────────────────────────────────────
  time_candidates <- c('unix_time', 'time', 'timestamp', 't', 'unix', 'epoch')
  time_col <- .resolve_col(col_time, time_candidates, 'time')

  if (time_col != 'unix_time') {
    nms_lower <- tolower(names(out))
    multi <- intersect(time_candidates[-1], nms_lower[nms_lower != tolower(time_col)])
    if (length(multi) > 0 && is.null(col_time))
      warning(
        "as_motion_trace(): multiple time-like columns found (",
        paste(names(out)[tolower(names(out)) %in% time_candidates], collapse = ', '),
        "). Using '", time_col, "'. Pass col_time to be explicit.",
        call. = FALSE
      )
    names(out)[names(out) == time_col] <- 'unix_time'
  }

  if (!is.numeric(out$unix_time)) {
    converted <- suppressWarnings(as.numeric(out$unix_time))
    if (all(is.na(converted)))
      stop(
        "as_motion_trace(): 'unix_time' could not be coerced to numeric. ",
        "Provide a column of Unix epoch seconds."
      )
    out$unix_time <- converted
  }

  # ── Resolve coordinate columns ───────────────────────────────────────────────
  if (coord_system == 'gps') {
    lat_col <- .resolve_col(col_lat, c('lat', 'latitude'),              'lat')
    lng_col <- .resolve_col(col_lng, c('lng', 'lon', 'longitude'),      'lng')
    if (lat_col != 'lat') names(out)[names(out) == lat_col] <- 'lat'
    if (lng_col != 'lng') names(out)[names(out) == lng_col] <- 'lng'
  } else {
    x_col <- .resolve_col(col_x, 'x', 'x')
    y_col <- .resolve_col(col_y, 'y', 'y')
    z_col <- .resolve_col(col_z, 'z', 'z', required = FALSE)
    if (x_col != 'x') names(out)[names(out) == x_col] <- 'x'
    if (y_col != 'y') names(out)[names(out) == y_col] <- 'y'
    if (!is.null(z_col) && z_col != 'z') names(out)[names(out) == z_col] <- 'z'
  }

  # ── Ensure is_interpolated exists ──────────────────────────────────────────
  if (!'is_interpolated' %in% names(out)) out$is_interpolated <- FALSE

  # ── Metadata and quality log ────────────────────────────────────────────────
  metadata <- create_metadata(
    session      = name,
    source       = 'as_motion_trace',
    trace        = out,
    device_info  = NULL,
    coord_system = coord_system
  )

  qual <- init_quality_log(out, 'as_motion_trace')

  out <- structure(
    out,
    class    = c('motion_trace', 'tbl_df', 'tbl', 'data.frame'),
    metadata = metadata,
    quality  = qual
  )

  extra_meta <- list(...)
  if (length(extra_meta) > 0) {
    out <- do.call(set_metadata, c(list(out), extra_meta))
  }

  out
}

#' Normalise Unix Timestamps
#' @keywords internal
norm_unix <- function(unix_vec){

  valid_times <- unix_vec[!is.na(unix_vec)]

  if(length(valid_times) == 0){
    return(unix_vec)
  }

  median_time <- median(valid_times, na.rm = TRUE)
  digits <- floor(log10(abs(median_time))) + 1

  if(digits == 13){
    return(unix_vec / 1000)
  } else if(digits == 11 || digits == 12){
    return(unix_vec / 100)
  } else if(digits == 9 || digits == 10){
    return(unix_vec)
  } else {
    warning(sprintf("Unexpected unix_time format (%d digits). Expected 9-13 digits. Returning unchanged.", digits))
    return(unix_vec)
  }
}

#' Convert to Unix Time
#' @keywords internal
convert_to_unix <- function(time_vec){

  valid_times <- time_vec[!is.na(time_vec)]

  if(length(valid_times) == 0){
    return(as.numeric(time_vec))
  }

  numeric_vec <- suppressWarnings(as.numeric(time_vec))

  if(sum(!is.na(numeric_vec)) > length(valid_times) * 0.8){
    valid_numeric <- numeric_vec[!is.na(numeric_vec)]
    median_val <- median(valid_numeric, na.rm = TRUE)

    if(median_val > 100000){
      return(norm_unix(numeric_vec))
    } else {
      return(numeric_vec)
    }
  }

  tryCatch({
    parsed <- lubridate::parse_date_time(time_vec,
                                          orders = c("ymd HMS", "dmy HMS", "mdy HMS",
                                                    "ymd HM", "dmy HM", "mdy HM",
                                                    "ymd", "dmy", "mdy"))
    unix_time <- as.numeric(parsed)

    if(sum(!is.na(unix_time)) > length(valid_times) * 0.5){
      return(unix_time)
    } else {
      warning("Could not parse time column as datetime. Returning as numeric.")
      return(as.numeric(time_vec))
    }
  }, error = function(e){
    warning(sprintf("Time conversion failed: %s. Using numeric conversion.", e$message))
    return(as.numeric(time_vec))
  })
}


# segment name: quality_log ---

#' Initialise Quality Log
#' @keywords internal
init_quality_log <- function(trace, source){

  time_diffs <- diff(trace$unix_time)
  time_diffs <- time_diffs[!is.na(time_diffs) & time_diffs > 0]

  if(length(time_diffs) > 0){
    median_dt <- median(time_diffs)
    native_hz <- round(1 / median_dt, 2)
  } else {
    median_dt <- NA
    native_hz <- NA
  }

  if(!is.na(median_dt)){
    gap_threshold <- median_dt * 3
    gaps <- which(time_diffs > gap_threshold)
    largest_gap <- if(length(gaps) > 0) max(time_diffs[gaps]) else 0
  } else {
    gaps <- integer(0)
    largest_gap <- 0
  }

  if ('lat' %in% names(trace) && 'lng' %in% names(trace)) {
    valid_coords <- sum(!is.na(trace$lat) & !is.na(trace$lng))
  } else if ('x' %in% names(trace) && 'y' %in% names(trace)) {
    valid_coords <- sum(!is.na(trace$x) & !is.na(trace$y))
  } else {
    valid_coords <- nrow(trace)
  }
  completeness <- valid_coords / nrow(trace)

  time_range <- range(trace$unix_time, na.rm = TRUE)
  duration_sec <- diff(time_range)

  issues <- list()
  warnings <- list()

  if(completeness < 0.90){
    issues <- c(issues, sprintf("Low coordinate completeness: %.1f%%", completeness * 100))
  } else if(completeness < 0.95){
    warnings <- c(warnings, sprintf("Moderate missing coordinates: %.1f%%", (1 - completeness) * 100))
  }

  if(!is.na(native_hz)){
    if(native_hz < 1){
      issues <- c(issues, sprintf("Very low sampling rate: %.2f Hz", native_hz))
    } else if(native_hz < 5){
      warnings <- c(warnings, sprintf("Low sampling rate: %.2f Hz (recommend ≥5 Hz)", native_hz))
    }
  }

  gap_percentage <- if(length(time_diffs) > 0) length(gaps) / length(time_diffs) * 100 else 0
  if(gap_percentage > 5){
    issues <- c(issues, sprintf("High gap frequency: %d gaps (%.1f%% of intervals)",
                                length(gaps), gap_percentage))
  } else if(gap_percentage > 2){
    warnings <- c(warnings, sprintf("Moderate gap frequency: %d gaps detected", length(gaps)))
  }

  if(largest_gap > 10){
    issues <- c(issues, sprintf("Large temporal gap detected: %.1f seconds", largest_gap))
  } else if(largest_gap > 5){
    warnings <- c(warnings, sprintf("Notable gap detected: %.1f seconds", largest_gap))
  }

  if(!is.na(duration_sec)){
    if(duration_sec < 60){
      warnings <- c(warnings, sprintf("Very short session: %.0f seconds", duration_sec))
    } else if(duration_sec > 7200){
      warnings <- c(warnings, sprintf("Very long session: %.1f hours", duration_sec / 3600))
    }
  }

  if(source %in% c('strava', 'guess_csv', 'catapult_replay', 'manual_csv', 'gpx', 'template_csv') &&
     'lat' %in% names(trace) && 'lng' %in% names(trace)){
    lat_range <- diff(range(trace$lat, na.rm = TRUE))
    lng_range <- diff(range(trace$lng, na.rm = TRUE))

    if(lat_range < 0.0001 && lng_range < 0.0001){
      warnings <- c(warnings, "Very small coordinate range - indoor/stationary session?")
    }

    invalid_lat <- sum(abs(trace$lat) > 90, na.rm = TRUE)
    invalid_lng <- sum(abs(trace$lng) > 180, na.rm = TRUE)

    if(invalid_lat > 0 || invalid_lng > 0){
      issues <- c(issues, sprintf("Invalid GPS coordinates: %d latitude, %d longitude",
                                  invalid_lat, invalid_lng))
    }
  }

  n_duplicates <- sum(duplicated(trace$unix_time))
  if(n_duplicates > 0){
    if(n_duplicates > nrow(trace) * 0.01){
      issues <- c(issues, sprintf("High duplicate timestamps: %d (%.1f%%)",
                                  n_duplicates, n_duplicates / nrow(trace) * 100))
    } else {
      warnings <- c(warnings, sprintf("Duplicate timestamps: %d points", n_duplicates))
    }
  }

  qc_pass <- length(issues) == 0

  initiate_entry <- list(
    step            = "initiate",
    step_id         = .mg_step_id(),
    package_version = .mg_pkg_version(),
    timestamp       = .mg_timestamp(),
    source          = source,

    rows_before = 0L,
    rows_after  = nrow(trace),
    cols_before = character(0),
    cols_after  = names(trace),

    total_rows = nrow(trace),
    valid_coordinates = valid_coords,
    completeness = completeness,

    native_hz = native_hz,
    median_dt = median_dt,
    duration_sec = as.numeric(duration_sec),
    time_range_unix = time_range,

    n_gaps = length(gaps),
    largest_gap_sec = largest_gap,
    gap_percentage = gap_percentage,

    lat_range = if('lat' %in% names(trace)) diff(range(trace$lat, na.rm = TRUE)) else NA,
    lng_range = if('lng' %in% names(trace)) diff(range(trace$lng, na.rm = TRUE)) else NA,

    n_duplicates = n_duplicates,

    column_mapping = attr(trace, 'metadata')$column_mapping,

    qc_pass = qc_pass,
    issues = if(length(issues) > 0) unlist(issues) else character(0),
    warnings = if(length(warnings) > 0) unlist(warnings) else character(0),

    qc_thresholds = list(
      completeness_issue = 0.90,
      completeness_warning = 0.95,
      hz_issue = 1,
      hz_warning = 5,
      gap_multiplier = 3,
      gap_pct_issue = 5,
      gap_pct_warning = 2,
      largest_gap_issue_sec = 10,
      largest_gap_warning_sec = 5,
      duplicate_pct_issue = 0.01,
      coord_range_threshold = 0.0001
    )
  )

  quality_log <- list(
    initiate = list(initiate_entry)
  )

  return(quality_log)
}

#' Quality Report

#' @param .data A \code{motion_trace} object.
#' @param step Optional character vector of step name(s) to display, e.g.
#'   \code{"initiate"}, \code{c("elaborate", "allocate")}. \code{NULL}
#'   (default) shows all steps.
#' @export
quality_report <- function(.data, step = NULL){

  qual <- attr(.data, 'quality')
  meta <- attr(.data, 'metadata')

  if(is.null(qual)){
    cat("No quality log found.\n")
    return(invisible(NULL))
  }

  cat("═══════════════════════════════════════════════════════════\n")
  cat("            MOTION TRACE QUALITY REPORT                    \n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  if(!is.null(meta)){
    cat(sprintf("Session: %s\n", meta$name))
    cat(sprintf("Source:  %s\n", meta$source))
    if(isTRUE(!is.na(meta$player_id))){
      cat(sprintf("Player:  %s\n", meta$player_id))
    }
    cat("\n")
  }

  all_steps <- names(qual)
  cat(sprintf("Processing: %s\n\n", paste(all_steps, collapse = " → ")))

  steps <- if (!is.null(step)) {
    unknown <- setdiff(step, all_steps)
    if (length(unknown) > 0) {
      warning("quality_report(): step(s) not found in quality log: ",
              paste(unknown, collapse = ", "))
    }
    intersect(all_steps, step)
  } else {
    all_steps
  }

  if (length(steps) == 0) {
    cat("No matching steps to display.\n")
    return(invisible(qual))
  }

  # Steps that store a list of run entries (one entry appended per call).
  # filtrate / allocate / elaborate use their own nested structures.
  run_tracked <- c('initiate', 'coordinate', 'interpolate', 'derivate',
                   'designate', 'quantitate')

  for(step_name in steps){
    # Resolve the most recent entry (or use the structure directly for steps
    # that manage their own multi-entry format).
    if (step_name %in% run_tracked) {
      run_entries <- qual[[step_name]]
      n_runs      <- length(run_entries)
      s           <- run_entries[[n_runs]]
    } else {
      s      <- qual[[step_name]]
      n_runs <- 1L
    }

    cat("───────────────────────────────────────────────────────────\n")
    cat(sprintf(" %s\n", toupper(step_name)))
    cat("───────────────────────────────────────────────────────────\n")
    if (n_runs > 1L) {
      cat(sprintf("  [%d run(s) recorded — showing most recent]\n\n", n_runs))
    }

    if(step_name == "initiate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Rows:          %s\n", format(s$total_rows, big.mark = ",")))
      cat(sprintf("  Completeness:  %.1f%% (%s valid coordinates)\n",
                  s$completeness * 100,
                  format(s$valid_coordinates, big.mark = ",")))

      if(isTRUE(!is.na(s$native_hz))){
        cat(sprintf("  Sample Rate:   %.2f Hz (%.3f s median interval)\n",
                    s$native_hz, s$median_dt))
      }

      cat(sprintf("  Duration:      %.1f minutes\n", s$duration_sec / 60))

      if(isTRUE(s$n_gaps > 0)){
        cat(sprintf("  Gaps:          %d (%.1f%% of intervals, largest: %.1fs)\n",
                    s$n_gaps, s$gap_percentage, s$largest_gap_sec))
      } else {
        cat("  Gaps:          None detected\n")
      }

      if(isTRUE(s$n_duplicates > 0)){
        cat(sprintf("  Duplicates:    %d timestamps\n", s$n_duplicates))
      }

      cat("\n")

      if(s$qc_pass){
        cat("  Status: ✓ PASS\n")
      } else {
        cat("  Status: ✗ ISSUES DETECTED\n")
      }

      if(length(s$issues) > 0){
        cat("\n  Issues:\n")
        for(issue in s$issues){
          cat(sprintf("    ✗ %s\n", issue))
        }
      }

      if(length(s$warnings) > 0){
        cat("\n  Warnings:\n")
        for(warning in s$warnings){
          cat(sprintf("    ⚠ %s\n", warning))
        }
      }

      cat("\n")
    }

    else if(step_name == "coordinate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Rows:          %s\n", format(s$total_rows, big.mark = ",")))
      cat("\n")

      # Conversion path
      cat("  Conversion Path\n")
      cat(sprintf("    From:        %s\n", s$from))
      cat(sprintf("    To:          %s\n", s$to))
      if(isTRUE(s$latlong_to_xyz)){
        cat("    Method:      Lat/Lng → projected XYZ (via sf)\n")
      } else {
        cat("    Method:      XYZ passthrough\n")
      }
      cat("\n")

      # CRS
      cat("  Coordinate Reference System\n")
      if(!is.na(s$crs_code)){
        cat(sprintf("    Source CRS:  EPSG:%s (WGS 84)\n", s$crs_source))
        cat(sprintf("    Target CRS:  EPSG:%s\n", s$crs_target))
        cat(sprintf("    Description: %s\n", s$crs_description))
      } else {
        cat("    CRS:         N/A (local coordinates)\n")
      }
      cat("\n")

      # Units
      cat("  Unit Conversion\n")
      if(isTRUE(s$units_converted)){
        cat(sprintf("    Converted:   %s → %s\n", s$from_units, s$to_units))
      } else {
        cat(sprintf("    Units:       %s (no conversion applied)\n", s$to_units))
      }
      cat("\n")

      # Normalisation
      cat("  Normalisation\n")
      if(isTRUE(s$normalised)){
        cat(sprintf("    Applied:     Yes (%s)\n", s$origin_type))
      } else {
        cat("    Applied:     No\n")
      }
      cat("\n")

      # Rotation
      cat("  Rotation\n")
      if(isTRUE(s$rotated)){
        cat(sprintf("    Applied:     Yes (%.4f rad / %.2f°)\n",
                    s$rotation_angle, s$rotation_degrees))
      } else {
        cat("    Applied:     No\n")
      }
      cat("\n")

      # Z dimension
      cat("  Dimensions\n")
      cat(sprintf("    X range:     %.2f %s\n", s$x_range, s$to_units))
      cat(sprintf("    Y range:     %.2f %s\n", s$y_range, s$to_units))
      cat(sprintf("    Z data:      %s\n", if(isTRUE(s$z_present)) "Present (non-zero)" else "Absent or zero-filled"))
      cat("\n")

      # Outliers
      cat("  Spatial Outliers\n")
      if(length(s$outliers) == 0){
        cat("    None detected\n")
      } else {
        cat(sprintf("    Total flagged rows: %s (%.1f%%)\n",
                    format(s$n_outlier_rows, big.mark = ","),
                    s$n_outlier_rows / s$total_rows * 100))
        cat("\n")
        for(ol_name in names(s$outliers)){
          ol <- s$outliers[[ol_name]]
          cat(sprintf("    [%s] %s\n", toupper(ol_name), ol$description))
          cat(sprintf("      Rows affected: %s (%.1f%%)\n",
                      format(ol$n, big.mark = ","), ol$pct))
          if(!is.null(ol$bounds)){
            cat(sprintf("      Bounds:        %.2f to %.2f %s\n",
                        ol$bounds[1], ol$bounds[2], s$to_units))
          }
        }
      }
      cat("\n")

      # Dependencies
      cat("  Package Dependencies\n")
      for(pkg_name in names(s$dependencies)){
        cat(sprintf("    %-10s v%s\n", pkg_name, s$dependencies[[pkg_name]]))
      }
      cat("\n")
    }

    else if(step_name == "interpolate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Rows in:       %s\n", format(s$input_rows, big.mark = ",")))
      cat(sprintf("  Rows out:      %s\n", format(s$total_rows, big.mark = ",")))
      cat("\n")

      # Method
      cat("  Method\n")
      cat(sprintf("    Algorithm:   %s\n", s$method$algorithm))
      cat(sprintf("    Function:    %s\n", s$method$zoo_fn))
      cat(sprintf("    Description: %s\n", s$method$description))
      cat("\n")

      # Parameters
      cat("  Parameters\n")
      cat(sprintf("    Hz:              %s\n", s$parameters$hz))
      cat(sprintf("    Interval:        %s s\n", s$parameters$interval_sec))
      cat(sprintf("    Max gap frames:  %s\n", s$parameters$max_gap_frames))
      cat("\n")

      # Grid expansion
      cat("  Grid Expansion\n")
      cat(sprintf("    Duration:        %.1f s (%.1f min)\n",
                  s$duration_sec, s$duration_sec / 60))
      cat(sprintf("    Expected rows:   %s\n", format(s$expected_rows, big.mark = ",")))
      cat(sprintf("    Grid rows:       %s\n", format(s$grid_rows, big.mark = ",")))
      cat(sprintf("    Time gaps added: %s\n", format(s$time_gaps_added, big.mark = ",")))
      if(s$duplicates_removed > 0){
        cat(sprintf("    Duplicates removed: %d\n", s$duplicates_removed))
      }
      cat(sprintf("    Upstream coord NAs: %d\n", s$coord_na_from_upstream))
      cat("\n")

      # Gap structure
      cat("  Gap Structure (pre-interpolation)\n")
      if(s$gaps$n_gaps > 0){
        cat(sprintf("    Gaps:        %d\n", s$gaps$n_gaps))
        cat(sprintf("    Total NA:    %d frames\n", s$gaps$total_missing))
        cat(sprintf("    Min gap:     %d frames\n", s$gaps$min_gap))
        cat(sprintf("    Max gap:     %d frames\n", s$gaps$max_gap))
        cat(sprintf("    Mean gap:    %.2f frames\n", s$gaps$mean_gap))
        cat(sprintf("    Median gap:  %s frames\n", s$gaps$median_gap))
        if(!is.null(s$gaps$gaps_exceeding_maxgap) && s$gaps$gaps_exceeding_maxgap > 0){
          cat(sprintf("    Exceeded max_gap: %d gap(s)\n", s$gaps$gaps_exceeding_maxgap))
        }
      } else {
        cat("    No gaps detected\n")
      }
      cat("\n")

      # Interpolation results
      cat("  Results\n")
      cat(sprintf("    Interpolated:    %s rows (%.2f%%)\n",
                  format(s$n_interpolated, big.mark = ","), s$pct_interpolated))
      cat(sprintf("    Filled x:        %d\n", s$filled$x))
      cat(sprintf("    Filled y:        %d\n", s$filled$y))
      if(!is.null(s$filled$z) && s$filled$z > 0){
        cat(sprintf("    Filled z:        %d\n", s$filled$z))
      }
      if(s$remaining_na$x > 0 || s$remaining_na$y > 0){
        cat(sprintf("    Remaining NAs:   x=%d, y=%d\n",
                    s$remaining_na$x, s$remaining_na$y))
      }
      cat("\n")

      # Issues
      if(!is.null(s$issues) && length(s$issues) > 0){
        cat("  Issues:\n")
        for(issue in s$issues){
          cat(sprintf("    ⚠ %s\n", issue))
        }
        cat("\n")
      }

      # Dependencies
      cat("  Package Dependencies\n")
      for(pkg_name in names(s$dependencies)){
        cat(sprintf("    %-10s v%s\n", pkg_name, s$dependencies[[pkg_name]]))
      }
      cat("\n")
    }

    else if(step_name == "filtrate"){
      n_passes <- length(s$passes)
      cat(sprintf("  Filter passes: %d\n", n_passes))
      cat(sprintf("  Chain:         %s\n\n",
                  paste(vapply(s$passes, function(p) p$method, character(1)), collapse = " → ")))

      for(i in seq_along(s$passes)){
        p <- s$passes[[i]]
        cat(sprintf("  Pass %d: %s\n", i, toupper(p$method)))
        cat(sprintf("    Timestamp:   %s\n", .mg_fmt_ts(p$timestamp)))
        cat(sprintf("    Rows:        %s\n", format(p$total_rows, big.mark = ",")))

        # Parameters
        cat("    Parameters\n")
        for(pname in names(p$parameters)){
          cat(sprintf("      %-12s %s\n", paste0(pname, ":"), p$parameters[[pname]]))
        }

        # NAs introduced (relevant for edge-effect filters like SMA)
        total_na <- sum(p$na_introduced)
        if(total_na > 0){
          cat(sprintf("    NAs introduced: %d across %s\n",
                      total_na, paste(names(p$na_introduced), collapse = ", ")))
        } else {
          cat("    NAs introduced: None\n")
        }

        # Dependencies for this pass
        cat("    Dependencies\n")
        for(pkg_name in names(p$dependencies)){
          cat(sprintf("      %-10s v%s\n", pkg_name, p$dependencies[[pkg_name]]))
        }
        cat("\n")
      }
    }

    else if(step_name == "derivate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat("\n")

      # Coordinate source
      cat("  Coordinate Source\n")
      if(s$coordinate_source$used_filtered){
        cat(sprintf("    Used:        filtered (%s, %s)\n",
                    s$coordinate_source$x_col, s$coordinate_source$y_col))
      } else if(s$coordinate_source$requested_filtered){
        cat(sprintf("    Used:        raw (%s, %s)  [filtrate() not applied]\n",
                    s$coordinate_source$x_col, s$coordinate_source$y_col))
      } else {
        cat(sprintf("    Used:        raw (%s, %s)\n",
                    s$coordinate_source$x_col, s$coordinate_source$y_col))
      }
      cat("\n")

      # Windows
      p <- s$parameters
      cat("  Windows\n")
      cat(sprintf("    Default:          %d rows (1-second period at Hz)\n", p$window_default))
      cat(sprintf("    velocity:         %d rows\n", p$velocity))
      cat(sprintf("    acceleration:     %d rows\n", p$acceleration))
      cat(sprintf("    angular_velocity: %d rows\n", p$angular_velocity))
      cat("\n")

      # Other parameters
      cat("  Parameters\n")
      cat(sprintf("    speed_floor:  %.3f m/s\n", p$speed_floor))
      cat(sprintf("    signed_omega: %s\n", if(isTRUE(p$signed_omega)) "Yes" else "No"))
      cat("\n")

      # Output summaries
      cat("  Output Columns\n")
      for(col_name in names(s$outputs)){
        o <- s$outputs[[col_name]]
        if(!is.na(o$min)){
          cat(sprintf("    %-20s %s valid  [%.3g, %.3g]  %d NA\n",
                      col_name, format(o$n_valid, big.mark = ","),
                      o$min, o$max, o$n_na))
        } else {
          cat(sprintf("    %-20s %d NA\n", col_name, o$n_na))
        }
      }
      cat("\n")

      # Issues
      if(length(s$issues) > 0){
        cat("  Issues\n")
        for(issue in s$issues){
          cat(sprintf("    ⚠ %s\n", issue))
        }
        cat("\n")
      }
    }

    else if(step_name == "designate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat("\n")

      if(s$method == 'corbett'){
        a <- s$algorithm
        cat("  Method:  Corbett (automated change-point detection)\n\n")
        cat("  Algorithm\n")
        cat(sprintf("    Package:      changepoint v%s\n", a$package_version))
        cat(sprintf("    Function:     cpt.mean()\n"))
        cat(sprintf("    Segmentation: %s\n", a$segmentation))
        cat(sprintf("    Penalty:      %s\n", a$penalty))
        cat(sprintf("    Threshold:    %.3f m/s (active vs downtime)\n", a$v_threshold))
        cat(sprintf("    Min pts:      %d rows\n", a$min_pts))
      } else {
        cat("  Method:  Manual (CSV import)\n\n")
        cat(sprintf("    File:         %s\n", s$source_file))
        if(!is.null(s$cols_mapping)){
          if(identical(s$cols_mapping, 'guess')){
            cat("    Columns:      auto-guessed\n")
          } else if(is.character(s$cols_mapping) && !is.null(names(s$cols_mapping))){
            cat("    Column mapping:\n")
            for(k in names(s$cols_mapping)){
              cat(sprintf("      %-14s → %s\n", k, s$cols_mapping[[k]]))
            }
          }
        } else {
          cat("    Columns:      exact match (no mapping)\n")
        }
      }
      cat("\n")

      cat("  Coverage\n")
      cat(sprintf("    Rows designated: %s / %s (%.1f%%)\n",
                  format(s$n_designated, big.mark = ","),
                  format(s$n_total_rows, big.mark = ","),
                  s$pct_designated))
      cat("\n")

      cat(sprintf("  Segments (%d total)\n", s$n_segments))
      for(seg_name in names(s$segment_counts)){
        cat(sprintf("    %-20s %s rows\n",
                    seg_name,
                    format(as.integer(s$segment_counts[[seg_name]]), big.mark = ",")))
      }
      cat("\n")
    }

    else if(step_name == "allocate"){
      allocs <- s$allocations
      cat(sprintf("  Allocations applied: %d\n\n", length(allocs)))

      for(aname in names(allocs)){
        a <- allocs[[aname]]

        if(identical(a$source, 'csv')){
          cat(sprintf("  [%s]  imported from: %s\n", aname, basename(a$source_file)))
        } else {
          cat(sprintf("  [%s]  defined manually\n", aname))
        }

        for(deriv in names(a$bands)){
          b <- a$bands[[deriv]]
          cat(sprintf("    %-20s %d band%s   %s\n",
                      deriv,
                      b$n_bands,
                      if(b$n_bands == 1) "" else "s",
                      paste(b$ranges, collapse = ", ")))
        }
        cat("\n")
      }
    }

    else if(step_name == "elaborate"){
      # Each call to elaborate() appends one entry — show all of them,
      # since each represents a distinct column transformation.
      entries <- qual$elaborate
      cat(sprintf("  Calls: %d\n\n", length(entries)))

      for(i in seq_along(entries)){
        e <- entries[[i]]
        cat(sprintf("  Call %d  —  %s\n", i, .mg_fmt_ts(e$timestamp)))

        if(!is.null(e$by) && length(e$by) > 0){
          cat(sprintf("    Grouped by:  %s\n", paste(e$by, collapse = ', ')))
        } else {
          cat("    Grouped by:  (none)\n")
        }

        if(length(e$added) > 0){
          cat(sprintf("    Added:       %s\n", paste(e$added, collapse = ', ')))
        } else {
          cat("    Added:       (none)\n")
        }

        # Separate reserved overwrites from ordinary modifications
        overwritten_res <- e$overwritten_reserved %||% character(0)
        non_reserved    <- setdiff(e$modified, overwritten_res)

        if(length(non_reserved) > 0){
          cat(sprintf("    Modified:    %s\n", paste(non_reserved, collapse = ', ')))
        } else if(length(overwritten_res) == 0){
          cat("    Modified:    (none)\n")
        }

        if(length(overwritten_res) > 0){
          cat(sprintf("    ⚠ Overwrote: %s  [reserved pipeline column(s)]\n",
                      paste(overwritten_res, collapse = ', ')))
        }

        if(!is.null(e$expressions) && length(e$expressions) > 0){
          cat("    Expressions:\n")
          for(col_name in names(e$expressions)){
            cat(sprintf("      %-22s %s\n",
                        paste0(col_name, ":"), e$expressions[[col_name]]))
          }
        }

        cat("\n")
      }
    }

    else if(step_name == "quantitate"){
      cat(sprintf("  Timestamp:     %s\n", .mg_fmt_ts(s$timestamp)))
      cat(sprintf("  Scope:         %s\n", s$scope))
      cat(sprintf("  Allocation:    %s\n",
                  if (!is.null(s$allocation)) s$allocation else "(none)"))
      if (!is.null(s$derivative)) {
        cat(sprintf("  Derivative(s): %s\n", paste(s$derivative, collapse = ', ')))
      }
      cat(sprintf("  Rows in:       %s\n", format(s$rows_before, big.mark = ",")))
      cat(sprintf("  Rows out:      %s\n", format(s$rows_after,  big.mark = ",")))
      cat("\n")
    }
  }

  cat("═══════════════════════════════════════════════════════════\n")

  invisible(qual)
}

#' Metadata Report
#' @export
metadata_report <- function(.data){

  meta <- attr(.data, 'metadata')

  if(is.null(meta)){
    cat("No metadata found.\n")
    return(invisible(NULL))
  }

  cat("═══════════════════════════════════════════════════════════\n")
  cat("            MOTION TRACE METADATA REPORT                   \n")
  cat("═══════════════════════════════════════════════════════════\n\n")

  cat("SESSION INFORMATION ────────────────────────────────────\n")
  cat(sprintf("  Name:          %s\n", meta$name))
  cat(sprintf("  Source:        %s\n", meta$source))

  if(!is.na(meta$session_id)){
    cat(sprintf("  Session ID:    %s\n", meta$session_id))
  }

  if(!is.na(meta$session_start)){
    cat(sprintf("  Date:          %s\n", as.character(meta$session_start)))
  }

  if(!is.na(meta$session_duration_sec)){
    duration_min <- meta$session_duration_sec / 60
    if(duration_min < 60){
      cat(sprintf("  Duration:      %.1f minutes\n", duration_min))
    } else {
      duration_hr <- duration_min / 60
      cat(sprintf("  Duration:      %.1f hours\n", duration_hr))
    }
  }

  cat("\n")

  has_athlete_info <- !is.na(meta$player_id) || !is.na(meta$player_name) || !is.na(meta$team)

  if(has_athlete_info){
    cat("ATHLETE INFORMATION ────────────────────────────────────\n")

    if(!is.na(meta$player_id)){
      cat(sprintf("  Player ID:     %s\n", meta$player_id))
    }

    if(!is.na(meta$player_name)){
      cat(sprintf("  Player Name:   %s\n", meta$player_name))
    }

    if(!is.na(meta$team)){
      cat(sprintf("  Team:          %s\n", meta$team))
    }

    if(!is.na(meta$sport)){
      cat(sprintf("  Sport:         %s\n", meta$sport))
    }

    if(!is.na(meta$session_type)){
      cat(sprintf("  Session Type:  %s\n", meta$session_type))
    }

    cat("\n")
  }

  has_device_info <- !is.na(meta$device_type) || !is.na(meta$device_id) ||
                     !is.na(meta$device_manufacturer)

  if(has_device_info){
    cat("DEVICE INFORMATION ─────────────────────────────────────\n")

    if(!is.na(meta$device_type)){
      cat(sprintf("  Type:          %s\n", meta$device_type))
    }

    if(!is.na(meta$device_manufacturer)){
      cat(sprintf("  Manufacturer:  %s\n", meta$device_manufacturer))
    }

    if(!is.na(meta$device_id)){
      cat(sprintf("  Device ID:     %s\n", meta$device_id))
    }

    if(!is.na(meta$firmware_version)){
      cat(sprintf("  Firmware:      %s\n", meta$firmware_version))
    }

    cat("\n")
  }

  cat("TECHNICAL DETAILS ──────────────────────────────────────\n")

  cat(sprintf("  Coordinate System:  %s\n", toupper(meta$coordinate_system)))

  if(!is.na(meta$native_hz)){
    cat(sprintf("  Sample Rate:        %.2f Hz\n", meta$native_hz))
    cat(sprintf("  Sample Interval:    %.3f seconds\n", meta$median_dt))
  }

  if(!is.null(meta$filters_applied)){
    chain <- paste(meta$filters_applied, collapse = " → ")
    cat(sprintf("  Filters Applied:    %s\n", chain))
  }

  cat("\n")

  # COLUMN MAPPING
  if(!is.null(meta$column_mapping) && meta$source %in% c('guess_csv', 'manual_csv', 'template_csv', 'gpx')){
    cat("COLUMN MAPPING ─────────────────────────────────────────\n")

    mapping <- meta$column_mapping

    for(col_name in names(mapping)){
      original <- mapping[[col_name]]
      if(!is.null(original) && original != "[not specified]"){
        cat(sprintf("  %-14s → %s\n", col_name, original))
      } else if(original == "[not specified]"){
        cat(sprintf("  %-14s → [not specified]\n", col_name))
      } else {
        cat(sprintf("  %-14s → [not found]\n", col_name))
      }
    }

    cat("\n")
  }

  cat("PROCESSING INFORMATION ─────────────────────────────────\n")
  cat(sprintf("  Created:       %s\n", .mg_fmt_ts(meta$created_timestamp)))
  cat(sprintf("  Created By:    %s\n", meta$created_by))
  cat(sprintf("  Package Ver:   %s\n", meta$package_version))
  cat("\n")

  invisible(meta)
}

#' Full Report
#' @export
full_report <- function(.data){
  cat("\n")
  metadata_report(.data)
  cat("\n")
  quality_report(.data)
  cat("\n")
  invisible(.data)
}

# segment name: print_method ---

#' Print Motion Trace
#' @export
print.motion_trace <- function(x, ...){
  meta <- attr(x, 'metadata')
  qual <- attr(x, 'quality')

  if (is.null(meta)) {
    cat(sprintf("Motion Trace [no metadata] — %s rows, %s cols\n",
                format(nrow(x), big.mark = ","), ncol(x)))
    return(invisible(x))
  }

  cat(sprintf("Motion Trace (%s)\n", meta$source))
  cat(sprintf("├─ Session: %s\n", meta$name))

  if(isTRUE(!is.na(meta$player_id))){
    cat(sprintf("├─ Player:  %s\n", meta$player_id))
  }

  if(isTRUE(!is.na(meta$native_hz))){
    cat(sprintf("├─ Sample:  %.1f Hz\n", meta$native_hz))
  }

  cat(sprintf("├─ Rows:    %s\n", format(nrow(x), big.mark = ",")))
  all_cols <- colnames(x)
  n_cols <- length(all_cols)
  if (n_cols > 15) {
    shown <- paste(all_cols[seq_len(12)], collapse = ', ')
    cat(sprintf("├─ Columns: %s ... + %d more\n", shown, n_cols - 12L))
  } else {
    cat(sprintf("├─ Columns: %s\n", paste(all_cols, collapse = ', ')))
  }

  if(!is.null(qual) && !is.null(qual$initiate)){
    latest_initiate <- qual$initiate[[length(qual$initiate)]]
    if(isTRUE(latest_initiate$qc_pass)){
      cat("└─ QC:      ✓ Pass\n")
    } else {
      n_issues <- length(latest_initiate$issues)
      cat(sprintf("└─ QC:      ✗ %d issue%s detected\n",
                  n_issues, if(n_issues > 1) "s" else ""))
    }
  } else {
    cat("└─ QC:      Not evaluated\n")
  }

  invisible(x)
}

# segment name: initiate ---

#' Initiate a Motion Grammar Session
#'
#' @param source Character; data source. One of:
#'   \describe{
#'     \item{\code{'auto'}}{Infer source from file extension (\code{.csv} →
#'       \code{guess_csv}, \code{.gpx} → \code{gpx}).}
#'     \item{\code{'guess_csv'}}{\strong{Exploration only.} Fuzzy column
#'       detection — convenient for inspecting an unfamiliar file, but the
#'       resolved schema can change if column names or file layout change.
#'       Use \code{\link{guess_csv_template}()} to capture the detected schema
#'       and then switch to \code{'template_csv'} for reproducible pipelines.}
#'     \item{\code{'template_csv'}}{\strong{Reproducible / production.} Parses
#'       according to an explicit \code{\link{csv_template}} specification.
#'       Column mapping is locked and guaranteed to be identical across
#'       systems and time. Preferred for any analysis that will be reported
#'       or shared.}
#'     \item{\code{'manual_csv'}}{Specify column names via \code{...} args;
#'       intermediate between guessing and a saved template.}
#'     \item{\code{'strava'}}{Strava activity (requires auth).}
#'     \item{\code{'catapult_replay'}}{Catapult CSV replay export.}
#'     \item{\code{'gpx'}}{Standard GPX track file.}
#'     \item{\code{'generic'}}{Any JSON REST API via an
#'       \code{\link{api_stream_template}}.}
#'   }
#' @param session Character; Strava ID/URL or file path
#' @param coord_system Character; 'gps' or 'local'
#' @param verbose Logical; print summary
#' @param template A \code{csv_template} object created by
#'   \code{\link{csv_template}()}, or a file path to an XML template saved by
#'   \code{\link{save_csv_template}()}. When provided, \code{source} is
#'   automatically set to \code{'template_csv'}.
#' @param ... Additional arguments for manual_csv
#' @export
initiate <- function(source = 'auto',
                     session = NULL,
                     coord_system = 'gps',
                     verbose = TRUE,
                     template = NULL,
                     ...){

  if (is.null(session)) {
    stop("initiate(): 'session' must be provided. Supply a file path (CSV/GPX) or a Strava activity ID.")
  }

  # Resolve template: XML file path -> csv_template or api_stream_template
  if (is.character(template) && length(template) == 1) {
    if (!file.exists(template))
      stop("initiate(): template file not found: ", template)
    xml_content <- paste(readLines(template, warn = FALSE), collapse = "\n")
    if (grepl("<api_stream_template>", xml_content, fixed = TRUE)) {
      template <- load_api_stream_template(template)
    } else {
      template <- load_csv_template(template)
    }
  }

  # Auto-detect source
  if (!is.null(template)) {
    if (inherits(template, "csv_template")) {
      if (source != 'auto' && source != 'template_csv')
        warning("initiate(): template provided — ignoring source = '", source,
                "' and using 'template_csv'")
      source <- 'template_csv'
    } else if (inherits(template, "api_stream_template")) {
      if (source != 'auto' && source != template$api)
        warning("initiate(): template provided — ignoring source = '", source,
                "' and using '", template$api, "'")
      source <- template$api
    }
  } else if (source == 'auto' && file.exists(session)) {
    ext <- tolower(tools::file_ext(session))
    source <- switch(ext,
      'csv' = 'guess_csv',
      'gpx' = 'gpx',
      stop(sprintf("Unknown file extension: .%s\nSupported: .csv, .gpx", ext))
    )
    if (verbose) message(sprintf("Auto-detected source: %s", source))
  }

  trace <- switch(source,
    'strava' = {
      session_str <- as.character(session)
      activity_id <- if(stringr::str_detect(session_str, 'activities/')){
        stringr::str_extract(session_str, '(?<=activities/)\\d+')
      } else {
        stringr::str_extract(session_str, '^\\d+$')
      }

      api_tmpl <- if (inherits(template, "api_stream_template")) template else NULL
      tok_path <- if (!is.null(api_tmpl)) api_tmpl$token_path else '~/strava_tokens.json'
      if (!is.null(api_tmpl)) coord_system <- api_tmpl$coord_system

      tokens <- get_valid_tokens(
        jsonlite::fromJSON(path.expand(tok_path)),
        verbose = verbose,
        token_path = tok_path
      )

      meta <- httr2::resp_body_json(
        .safe_perform(
          httr2::request(paste0('https://www.strava.com/api/v3/activities/', activity_id)) |>
            httr2::req_auth_bearer_token(tokens$access_token),
          "Strava activity fetch"
        )
      )

      trace <- get_physics_streams(activity_id, tokens$access_token, meta$start_date,
                                   template = api_tmpl)

      metadata <- create_metadata(
        session = activity_id,
        source = 'strava',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
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
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      all_lines <- readLines(session)
      header_idx <- grep('^Time, Time, Latitude', all_lines)[1]

      after_header <- all_lines[(header_idx + 1):length(all_lines)]
      empty_idx <- which(after_header == '')[1]
      n_rows <- if(is.na(empty_idx)) -1 else empty_idx - 1

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
          lat = 2,
          lng = 3
        ) |>
        dplyr::mutate(
          unix_time = convert_to_unix(unix_raw),
          altitude = NA_real_
        ) |>
        dplyr::select(unix_time, lat, lng, altitude)

      start_line <- all_lines[grep('StartTimeSeconds', all_lines)]
      start_time_raw <- stringr::str_extract(start_line, '(?<=StartTimeSeconds ).*')

      metadata <- create_metadata(
        session = session,
        source = 'catapult_replay',
        trace = trace,
        device_info = list(type = 'Catapult'),
        coord_system = 'gps'
      )

      if(length(start_time_raw) > 0 && !is.na(start_time_raw[1])){
        parsed_start <- tryCatch({
          lubridate::parse_date_time(start_time_raw[1], orders = c('dmy HMS', 'mdy HMS'))
        }, error = function(e) NA)

        if(!is.na(parsed_start)){
          metadata$session_start <- as.Date(parsed_start)
        }
      }

      attr(trace, 'metadata') <- metadata
      trace
    },

    'guess_csv' = {
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      trace <- initiate_guess_csv(session, source = coord_system)

      # Extract column mapping from trace
      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'guess_csv',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      # Add column mapping to metadata
      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata

      # Clean up temporary attribute
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'manual_csv' = {
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      trace <- initiate_manual_csv(.data_path = session, coord_system = coord_system, ...)

      # Extract column mapping from trace
      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'manual_csv',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      # Add column mapping to metadata
      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata

      # Clean up temporary attribute
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'template_csv' = {
      if (is.null(template))
        stop("initiate(): source = 'template_csv' requires a template argument")
      if (!file.exists(session))
        stop(sprintf("File not found: %s\nCheck path and try again.", session))

      coord_system <- template$coord_system  # template takes precedence

      trace <- initiate_template_csv(session, template)

      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'template_csv',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'gpx' = {
      if(!file.exists(session)){
        stop(sprintf("File not found: %s\nCheck path and try again.", session))
      }

      trace <- initiate_gpx(session)

      col_mapping <- attr(trace, 'column_mapping')

      metadata <- create_metadata(
        session = session,
        source = 'gpx',
        trace = trace,
        device_info = NULL,
        coord_system = 'gps'
      )

      metadata$column_mapping <- col_mapping

      attr(trace, 'metadata') <- metadata
      attr(trace, 'column_mapping') <- NULL

      trace
    },

    'generic' = {
      api_tmpl <- if (inherits(template, "api_stream_template")) template else NULL
      if (is.null(api_tmpl))
        stop("initiate(): source = 'generic' requires an api_stream_template passed via template")

      coord_system <- api_tmpl$coord_system
      trace <- fetch_generic_streams(session, api_tmpl)

      metadata <- create_metadata(
        session = session,
        source = 'generic',
        trace = trace,
        device_info = NULL,
        coord_system = coord_system
      )

      attr(trace, 'metadata') <- metadata
      trace
    },

    stop("Unknown source. Use 'auto', 'strava', 'catapult_replay', 'guess_csv', 'manual_csv', 'gpx', 'template_csv', or 'generic'.")
  )

  trace$is_interpolated <- FALSE

  attr(trace, 'quality') <- init_quality_log(trace, source)

  class(trace) <- c('motion_trace', class(trace))
  if(verbose) print(trace)

  return(trace)
}
