# segment name: initiate_logic ---

#' Auto-Detect and Parse Messy CSV Files
#' @keywords internal
initiate_guess_csv <- function(.data_path,
                               source = "gps",
                               confidence_threshold = 0.8){

  source <- match.arg(source, choices = c("gps", "local", "auto"))

  all_lines <- readr::read_lines(.data_path)
  comma_counts <- stringr::str_count(all_lines, ',')

  line_stats <- tibble::tibble(count = comma_counts) |>
    dplyr::filter(count > 0) |>
    dplyr::group_by(count) |>
    dplyr::summarise(n = dplyr::n(), .groups = 'drop') |>
    dplyr::arrange(dplyr::desc(n))

  if(nrow(line_stats) == 0){
    stop('No valid CSV structure detected in file.')
  }

  target_commas <- line_stats$count[1]
  first_data_idx <- which(comma_counts == target_commas)[1]

  if(is.na(first_data_idx)){
    stop('Could not identify data block in file.')
  }

  if(first_data_idx >= 2){
    # If the first line of the data block contains letters it IS the header
    # (standard device-export layout: preamble rows → header → data).
    # Only fall back to the backward preamble search for the rare case where
    # data rows have more commas than the header (e.g. trailing empty fields).
    if(stringr::str_detect(all_lines[first_data_idx], '^[A-Za-z_]')){
      header_idx <- first_data_idx
    } else {
      potential_headers <- all_lines[1:(first_data_idx - 1)]
      header_candidates <- which(stringr::str_detect(potential_headers, '[A-Za-z]'))

      if(length(header_candidates) == 0){
        stop('Could not identify a valid header row.')
      }

      header_idx <- utils::tail(header_candidates, 1)
    }
  } else {
    # Standard CSV: header is line 1, same comma count as data rows
    if(!stringr::str_detect(all_lines[1], '[A-Za-z]')){
      stop('Could not identify a valid header row.')
    }
    header_idx <- 1L
  }

  raw_header <- all_lines[header_idx]
  raw_cols <- raw_header |>
    stringr::str_split(',') |>
    purrr::pluck(1) |>
    stringr::str_trim() |>
    stringr::str_remove_all('^"|"$')   # strip surrounding quotes from write.csv output

  clean_names <- raw_cols[raw_cols != '']

  if(length(clean_names) == 0){
    stop('No valid column names found in header.')
  }

  clean_names <- make.unique(clean_names, sep = '_')
  data_lines <- all_lines[comma_counts == target_commas]

  # When the header shares the same comma count as data rows it is included in
  # data_lines; remove it so it isn't duplicated when prepended as raw_header
  if(header_idx == first_data_idx){
    data_lines <- data_lines[-1]
  }

  if(length(data_lines) == 0){
    stop('No data rows found matching the identified structure.')
  }

  df <- suppressWarnings(
    readr::read_csv(
      I(paste0(c(raw_header, data_lines), collapse = '\n')),
      show_col_types = FALSE,
      name_repair = 'minimal',
      col_types = readr::cols(.default = readr::col_character())
    )
  )

  df <- df[, 1:length(clean_names), drop = FALSE]
  colnames(df) <- clean_names

  if(source == "auto"){
    col_lower <- stringr::str_to_lower(colnames(df))

    has_lat <- any(stringr::str_detect(col_lower, 'lat'))
    has_lon <- any(stringr::str_detect(col_lower, 'lon'))
    has_x <- any(col_lower == 'x' | stringr::str_detect(col_lower, 'coord_x'))
    has_y <- any(col_lower == 'y' | stringr::str_detect(col_lower, 'coord_y'))

    if(has_lat || has_lon){
      source <- "gps"
    } else if(has_x || has_y){
      source <- "local"
    } else {
      source <- "gps"
    }

    message(sprintf("Auto-detected coordinate system: %s", source))
  }

  if(source == "gps"){
    targets <- list(
      unix_time = list(
        primary = c('unix', 'timestamp', 'unixtime', 'time_ms', 'utc', 'epoch'),
        fallback = c('time', 'datetime', 'date', 'frame', 'frameid', 'frame_id', 'frame_number')
      ),
      lat = list(
        primary = c('latitude', 'lat'),
        fallback = c('y', 'coord_y', 'lat_deg')
      ),
      lng = list(
        primary = c('longitude', 'lon', 'long', 'lng'),
        fallback = c('x', 'coord_x', 'lon_deg', 'long_deg')
      ),
      altitude = list(
        primary = c('altitude', 'elevation', 'height', 'alt', 'elev'),
        fallback = c('z', 'alt_m', 'elevation_m')
      )
    )
    output_cols <- c('unix_time', 'lat', 'lng', 'altitude')
  } else {
    targets <- list(
      unix_time = list(
        primary = c('unix', 'timestamp', 'unixtime', 'time_ms', 'utc'),
        fallback = c('time', 'datetime', 'date', 'epoch')
      ),
      x = list(
        primary = c('x', 'coord_x', 'pos_x'),
        fallback = c('longitude', 'lon', 'long')
      ),
      y = list(
        primary = c('y', 'coord_y', 'pos_y'),
        fallback = c('latitude', 'lat')
      ),
      z = list(
        primary = c('z', 'coord_z', 'pos_z', 'height'),
        fallback = c('altitude', 'elevation', 'alt')
      )
    )
    output_cols <- c('unix_time', 'x', 'y', 'z')
  }

  find_best_match <- function(target_patterns, available_cols){
    available_lower <- stringr::str_to_lower(available_cols)

    for(pattern in target_patterns){
      pattern_lower <- stringr::str_to_lower(pattern)
      exact_match <- which(available_lower == pattern_lower)
      if(length(exact_match) > 0){
        return(available_cols[exact_match[1]])
      }
    }

    best_score <- 0
    best_match <- NULL

    for(pattern in target_patterns){
      pattern_lower <- stringr::str_to_lower(pattern)

      for(i in seq_along(available_cols)){
        col_lower <- available_lower[i]
        sim <- 1 - stringdist::stringdist(pattern_lower, col_lower, method = 'jw')

        if(sim > best_score && sim >= confidence_threshold){
          best_score <- sim
          best_match <- available_cols[i]
        }
      }
    }

    return(best_match)
  }

  # Apply fuzzy mappings - with exclusion to prevent double-matching
  mappings <- list()
  available_cols <- colnames(df)

  for(target_name in names(targets)){
    match <- find_best_match(targets[[target_name]]$primary, available_cols)

    if(is.null(match) && !is.null(targets[[target_name]]$fallback)){
      match <- find_best_match(targets[[target_name]]$fallback, available_cols)
    }

    mappings[[target_name]] <- match

    # Remove matched column from pool to prevent double-matching
    if(!is.null(match)){
      available_cols <- setdiff(available_cols, match)
    }
  }

  for(target_name in names(mappings)){
    original_col <- mappings[[target_name]]

    if(!is.null(original_col) && original_col %in% colnames(df)){
      df <- df |>
        dplyr::rename(!!target_name := !!original_col)
    }
  }

  if('unix_time' %in% colnames(df)){
    df <- df |>
      dplyr::mutate(unix_time = convert_to_unix(unix_time))
  }

  other_cols <- setdiff(output_cols, 'unix_time')
  for(col in other_cols){
    if(col %in% colnames(df)){
      df <- df |>
        dplyr::mutate(!!col := suppressWarnings(as.numeric(.data[[col]])))
    }
  }

  output <- df |>
    dplyr::select(dplyr::any_of(output_cols))

  missing_cols <- setdiff(output_cols, colnames(output))
  for(col in missing_cols){
    output[[col]] <- NA_real_
  }

  output <- output |>
    dplyr::select(dplyr::all_of(output_cols))

  if(nrow(output) == 0){
    stop('No valid data rows after parsing.')
  }

  all_na <- all(sapply(output, function(x) all(is.na(x))))
  if(all_na){
    warning('All columns are NA - column matching may have failed. Check your column names.')
  }

  # Store column mapping as attribute for later retrieval
  attr(output, 'column_mapping') <- mappings
  attr(output, 'skip') <- as.integer(header_idx - 1L)
  attr(output, 'coord_system_detected') <- source

  return(output)
}

#' Manually Parse CSV Files
#' @keywords internal
initiate_manual_csv <- function(.data_path,
                               skip = 0,
                               col_unix,
                               col_lat = NULL,
                               col_lng = NULL,
                               col_altitude = NULL,
                               col_x = NULL,
                               col_y = NULL,
                               col_z = NULL,
                               coord_system = c("gps", "local"),
                               max_empty_lines = 3,
                               n_max = NULL,
                               comment = "#"){

  coord_system <- match.arg(coord_system)

  if(!file.exists(.data_path)){
    stop(sprintf("File not found: %s", .data_path))
  }

  if(missing(col_unix)) stop("col_unix must be specified")

  # For local mode accept col_x/col_y (preferred) or fall back to col_lat/col_lng
  # for backward compatibility. col_lat/col_lng remain the canonical names for GPS mode.
  if(coord_system == "local"){
    col_coord1 <- col_x %||% col_lat
    col_coord2 <- col_y %||% col_lng
    col_coord3 <- col_z %||% col_altitude
    if(is.null(col_coord1)) stop("col_x must be specified when coord_system = 'local'")
    if(is.null(col_coord2)) stop("col_y must be specified when coord_system = 'local'")
  } else {
    col_coord1 <- col_lat
    col_coord2 <- col_lng
    col_coord3 <- col_altitude
    if(is.null(col_coord1)) stop("col_lat must be specified when coord_system = 'gps'")
    if(is.null(col_coord2)) stop("col_lng must be specified when coord_system = 'gps'")
  }

  all_lines <- readr::read_lines(.data_path)

  if(skip > 0){
    all_lines <- all_lines[(skip + 1):length(all_lines)]
  }

  all_lines <- all_lines[!stringr::str_detect(all_lines, sprintf("^\\s*%s", comment))]

  empty_pattern <- stringr::str_detect(all_lines, "^\\s*$")

  consecutive_empty <- 0
  stop_at <- length(all_lines)

  for(i in seq_along(all_lines)){
    if(empty_pattern[i]){
      consecutive_empty <- consecutive_empty + 1
      if(consecutive_empty >= max_empty_lines){
        stop_at <- i - max_empty_lines
        break
      }
    } else {
      consecutive_empty <- 0
    }
  }

  data_to_read <- all_lines[1:stop_at]
  data_to_read <- data_to_read[!empty_pattern[1:stop_at]]

  df <- suppressWarnings(
    readr::read_csv(
      I(paste(data_to_read, collapse = "\n")),
      n_max = n_max,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
  )

  if(nrow(df) == 0){
    stop("No data rows found after parsing")
  }

  col_names_lower <- stringr::str_to_lower(colnames(df))

  find_idx <- function(col_name, label) {
    idx <- which(col_names_lower == stringr::str_to_lower(col_name))
    if(length(idx) == 0)
      stop(sprintf("Column '%s' not found. Available: %s",
                   col_name, paste(colnames(df), collapse = ", ")))
    idx
  }

  unix_idx   <- find_idx(col_unix,   "col_unix")
  coord1_idx <- find_idx(col_coord1, if(coord_system == "gps") "col_lat" else "col_x")
  coord2_idx <- find_idx(col_coord2, if(coord_system == "gps") "col_lng" else "col_y")

  coord3_idx <- NULL
  if(!is.null(col_coord3)){
    coord3_idx <- which(col_names_lower == stringr::str_to_lower(col_coord3))
    if(length(coord3_idx) == 0){
      third_label <- if(coord_system == "gps") "col_altitude" else "col_z"
      warning(sprintf("Column '%s' not found. Setting %s to NA.", col_coord3, third_label))
      coord3_idx <- NULL
    }
  }

  if(coord_system == "gps"){
    output_names <- c('unix_time', 'lat', 'lng', 'altitude')
  } else {
    output_names <- c('unix_time', 'x', 'y', 'z')
  }

  output <- tibble::tibble(
    unix_time = df[[unix_idx[1]]],
    coord1 = df[[coord1_idx[1]]],
    coord2 = df[[coord2_idx[1]]]
  )

  if(!is.null(coord3_idx) && length(coord3_idx) > 0){
    output$coord3 <- df[[coord3_idx[1]]]
  } else {
    output$coord3 <- NA_character_
  }

  colnames(output) <- output_names

  output <- output |>
    dplyr::mutate(unix_time = convert_to_unix(unix_time))

  output <- output |>
    dplyr::mutate(
      !!output_names[2] := suppressWarnings(as.numeric(.data[[output_names[2]]])),
      !!output_names[3] := suppressWarnings(as.numeric(.data[[output_names[3]]])),
      !!output_names[4] := suppressWarnings(as.numeric(.data[[output_names[4]]]))
    )

  if(nrow(output) == 0){
    stop('No valid data rows after parsing')
  }

  na_unix <- sum(is.na(output$unix_time))
  na_coord1 <- sum(is.na(output[[output_names[2]]]))
  na_coord2 <- sum(is.na(output[[output_names[3]]]))

  if(na_unix > 0){
    warning(sprintf("%d unix_time values are NA", na_unix))
  }
  if(na_coord1 > 0){
    warning(sprintf("%d %s values are NA", na_coord1, output_names[2]))
  }
  if(na_coord2 > 0){
    warning(sprintf("%d %s values are NA", na_coord2, output_names[3]))
  }

  # Store column mapping as attribute
  if(coord_system == "gps"){
    attr(output, 'column_mapping') <- list(
      unix_time = col_unix,
      lat = col_coord1,
      lng = col_coord2,
      altitude = col_coord3 %||% "[not specified]"
    )
  } else {
    attr(output, 'column_mapping') <- list(
      unix_time = col_unix,
      x = col_coord1,
      y = col_coord2,
      z = col_coord3 %||% "[not specified]"
    )
  }

  return(output)
}

#' Parse GPX Files
#' @keywords internal
initiate_gpx <- function(.data_path){

  if(!requireNamespace("sf", quietly = TRUE)){
    stop("sf package required for GPX files. Install with: install.packages('sf')")
  }

  gpx_data <- sf::st_read(.data_path, layer = "track_points", quiet = TRUE)

  coords <- sf::st_coordinates(gpx_data)

  trace <- tibble::tibble(
    unix_time = as.numeric(gpx_data$time),
    lat = coords[, 2],
    lng = coords[, 1],
    altitude = dplyr::coalesce(gpx_data$ele, NA_real_)
  )

  attr(trace, 'column_mapping') <- list(
    unix_time = "time",
    lat = "latitude",
    lng = "longitude",
    altitude = "ele"
  )

  return(trace)
}



# segment name: csv_template ---

# ── XML helpers (no external dependency) ──────────────────────────────────────

#' @keywords internal
.xml_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  s <- gsub(">", "&gt;",  s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s
}

#' @keywords internal
.xml_unescape <- function(s) {
  s <- gsub("&quot;", '"', s, fixed = TRUE)
  s <- gsub("&gt;",   ">", s, fixed = TRUE)
  s <- gsub("&lt;",   "<", s, fixed = TRUE)
  s <- gsub("&amp;",  "&", s, fixed = TRUE)
  s
}

# Parse a column reference from the XML-serialised string back to its original
# type: integer if the string is all digits, otherwise character.
#' @keywords internal
.parse_col_ref <- function(val) {
  if (is.null(val) || nchar(trimws(val)) == 0) return(NULL)
  n <- suppressWarnings(as.integer(trimws(val)))
  if (!is.na(n) && identical(trimws(val), as.character(n))) n else val
}

# ── csv_template constructor ───────────────────────────────────────────────────

#' Create a CSV Format Template
#'
#' Defines a reusable column-mapping and parsing specification for importing
#' CSV files with a known but non-standard layout. Pass the resulting object
#' to \code{\link{initiate}()} via the \code{template} argument, or serialise
#' it with \code{\link{save_csv_template}()}.
#'
#' Column references (\code{col_unix}, \code{col_lat}, etc.) accept either a
#' \strong{column name} (character, case-insensitive) or a \strong{1-based
#' integer column index}. Using an index is useful when the file has duplicate
#' column names or no header row.
#'
#' \code{col_extra} accepts a named or unnamed character or integer vector:
#' \itemize{
#'   \item \code{c("HeartRate")} — import by name; output column keeps the CSV name.
#'   \item \code{c(hr = "HeartRate")} — import by name; output column renamed to \code{hr}.
#'   \item \code{c(5L)} — import column 5 by index; output column named \code{col_5}.
#'   \item \code{c(heart_rate = 5L)} — import column 5; output column named \code{heart_rate}.
#' }
#'
#' @param coord_system Character; \code{"gps"} (lat/lng) or \code{"local"}
#'   (x/y). Defaults to \code{"gps"}.
#' @param col_unix Column name or 1-based integer index of the timestamp
#'   column. Required.
#' @param col_lat Column name or index of the latitude column
#'   (\code{coord_system = "gps"}). Required for GPS.
#' @param col_lng Column name or index of the longitude column
#'   (\code{coord_system = "gps"}). Required for GPS.
#' @param col_altitude Column name or index of the altitude column (GPS mode).
#'   Optional.
#' @param col_x Column name or index of the x-coordinate column
#'   (\code{coord_system = "local"}). Required for local.
#' @param col_y Column name or index of the y-coordinate column
#'   (\code{coord_system = "local"}). Required for local.
#' @param col_z Column name or index of the z-coordinate column (local mode).
#'   Optional.
#' @param col_velocity Column name or index of a pre-computed velocity column.
#'   When supplied it is imported as \code{velocity}, allowing you to skip
#'   \code{derivate()} for that metric.
#' @param col_acceleration Column name or index of a pre-computed acceleration
#'   column. Imported as \code{acceleration} when supplied.
#' @param col_extra Named or unnamed character or integer vector of additional
#'   columns to carry through as custom elaboration columns. See Details.
#' @param skip Integer; rows to skip \emph{before} the header row. Use this
#'   for files with device metadata or preamble above the data table. Defaults
#'   to \code{0}.
#' @param max_empty_lines Integer; consecutive blank rows that signal the end
#'   of the data block. Defaults to \code{3}.
#' @param comment Character; line-prefix marking comment rows to be ignored.
#'   Defaults to \code{"#"}.
#' @param duplicate_method Character; behaviour when a column \emph{name}
#'   matches more than one column in the file.
#'   \itemize{
#'     \item \code{"return_error"} (default) — stop with an informative message
#'       listing the duplicate positions; use an integer index instead.
#'     \item \code{"use_first"} — silently take the first matching column.
#'     \item \code{"use_second"} — take the second matching column (useful for
#'       files like Catapult where the second \code{Time} column is the GPS
#'       timestamp).
#'   }
#'
#' @return An object of class \code{csv_template}.
#' @seealso \code{\link{save_csv_template}}, \code{\link{load_csv_template}},
#'   \code{\link{initiate}}
#' @export
csv_template <- function(
    coord_system = c("gps", "local"),
    col_unix,
    col_lat = NULL,
    col_lng = NULL,
    col_altitude = NULL,
    col_x = NULL,
    col_y = NULL,
    col_z = NULL,
    col_velocity = NULL,
    col_acceleration = NULL,
    col_extra = NULL,
    skip = 0L,
    max_empty_lines = 3L,
    comment = "#",
    duplicate_method = c("return_error", "use_first", "use_second"),
    delimiter = ",",
    decimal = "."
) {
  coord_system <- match.arg(coord_system)
  duplicate_method <- match.arg(duplicate_method)

  if (!is.character(delimiter) || nchar(delimiter) != 1)
    stop("csv_template(): delimiter must be a single character (e.g. \",\", \";\", \"\\t\")")
  if (!decimal %in% c(".", ","))
    stop("csv_template(): decimal must be \".\" or \",\"")

  if (missing(col_unix) || is.null(col_unix))
    stop("csv_template(): col_unix is required")

  if (coord_system == "gps") {
    if (is.null(col_lat))
      stop("csv_template(): col_lat is required when coord_system = 'gps'")
    if (is.null(col_lng))
      stop("csv_template(): col_lng is required when coord_system = 'gps'")
    col_coord1 <- col_lat
    col_coord2 <- col_lng
    col_coord3 <- col_altitude
  } else {
    if (is.null(col_x))
      stop("csv_template(): col_x is required when coord_system = 'local'")
    if (is.null(col_y))
      stop("csv_template(): col_y is required when coord_system = 'local'")
    col_coord1 <- col_x
    col_coord2 <- col_y
    col_coord3 <- col_z
  }

  structure(
    list(
      coord_system = coord_system,
      skip = as.integer(skip),
      max_empty_lines = as.integer(max_empty_lines),
      comment = as.character(comment),
      duplicate_method = duplicate_method,
      delimiter = as.character(delimiter),
      decimal = as.character(decimal),
      col_unix = col_unix,
      col_coord1 = col_coord1,
      col_coord2 = col_coord2,
      col_coord3 = col_coord3,
      col_velocity = col_velocity,
      col_acceleration = col_acceleration,
      col_extra = col_extra
    ),
    class = "csv_template"
  )
}

#' @method print csv_template
#' @export
print.csv_template <- function(x, ...) {
  coord_names <- if (x$coord_system == "gps")
    c("lat", "lng", "altitude")
  else
    c("x", "y", "z")

  fmt_ref <- function(ref) {
    if (is.null(ref)) return(NULL)
    if (is.numeric(ref) || is.integer(ref)) sprintf("[col %d]", as.integer(ref))
    else as.character(ref)
  }

  cat(sprintf("CSV Template [%s]\n", x$coord_system))
  locale_str <- ""
  delim <- x$delimiter %||% ","
  dec   <- x$decimal   %||% "."
  if (delim != "," || dec != ".")
    locale_str <- sprintf("  |  delimiter: '%s'  |  decimal: '%s'", delim, dec)
  cat(sprintf(
    "  skip: %d  |  max_empty_lines: %d  |  comment: '%s'  |  duplicate_method: %s%s\n",
    x$skip, x$max_empty_lines, x$comment, x$duplicate_method, locale_str
  ))
  cat("  Column mapping:\n")
  cat(sprintf("    unix_time <- %s\n", fmt_ref(x$col_unix)))
  cat(sprintf("    %-12s <- %s\n", coord_names[1], fmt_ref(x$col_coord1)))
  cat(sprintf("    %-12s <- %s\n", coord_names[2], fmt_ref(x$col_coord2)))
  if (!is.null(x$col_coord3))
    cat(sprintf("    %-12s <- %s\n", coord_names[3], fmt_ref(x$col_coord3)))
  if (!is.null(x$col_velocity))
    cat(sprintf("    velocity <- %s\n", fmt_ref(x$col_velocity)))
  if (!is.null(x$col_acceleration))
    cat(sprintf("    acceleration <- %s\n", fmt_ref(x$col_acceleration)))
  if (!is.null(x$col_extra) && length(x$col_extra) > 0) {
    nms <- names(x$col_extra)
    parts <- vapply(seq_along(x$col_extra), function(i) {
      ref <- x$col_extra[[i]]
      nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL
      if (is.null(nm)) fmt_ref(ref)
      else sprintf("%s <- %s", nm, fmt_ref(ref))
    }, character(1))
    cat(sprintf("  Extra columns: %s\n", paste(parts, collapse = ", ")))
  }
  invisible(x)
}

# ── XML serialisation ──────────────────────────────────────────────────────────

#' Save a CSV Template to an XML File
#'
#' Serialises a \code{csv_template} object to a portable XML file that can be
#' shared across scripts, checked into version control, or loaded later with
#' \code{\link{load_csv_template}()}.
#'
#' @param template A \code{csv_template} object.
#' @param path Character; file path for the output \code{.xml} file.
#'
#' @return Invisibly returns \code{template}.
#' @seealso \code{\link{csv_template}}, \code{\link{load_csv_template}}
#' @export
save_csv_template <- function(template, path) {
  if (!inherits(template, "csv_template"))
    stop("save_csv_template(): template must be a csv_template object")

  add_tag <- function(name, value, indent = "  ") {
    if (is.null(value) || (length(value) == 1 && is.na(value)))
      return(character(0))
    sprintf("%s<%s>%s</%s>", indent, name, .xml_escape(as.character(value)), name)
  }

  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<csv_template>",
    add_tag("coord_system",    template$coord_system),
    add_tag("skip",            template$skip),
    add_tag("max_empty_lines", template$max_empty_lines),
    add_tag("comment",         template$comment),
    add_tag("duplicate_method", template$duplicate_method),
    add_tag("delimiter",       template$delimiter %||% ","),
    add_tag("decimal",         template$decimal   %||% "."),
    "  <columns>",
    add_tag("col_unix",         template$col_unix,         "    "),
    add_tag("col_coord1",       template$col_coord1,       "    "),
    add_tag("col_coord2",       template$col_coord2,       "    "),
    add_tag("col_coord3",       template$col_coord3,       "    "),
    add_tag("col_velocity",     template$col_velocity,     "    "),
    add_tag("col_acceleration", template$col_acceleration, "    "),
    "  </columns>"
  )

  if (!is.null(template$col_extra) && length(template$col_extra) > 0) {
    nms <- names(template$col_extra)
    extra_tags <- vapply(seq_along(template$col_extra), function(i) {
      ref <- template$col_extra[[i]]
      nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL
      as_attr <- if (!is.null(nm))
        sprintf(' as="%s"', .xml_escape(nm))
      else
        ""
      sprintf('    <col ref="%s"%s/>', .xml_escape(as.character(ref)), as_attr)
    }, character(1))
    lines <- c(lines, "  <extra_columns>", extra_tags, "  </extra_columns>")
  }

  lines <- c(lines, "</csv_template>")
  writeLines(lines, path)
  invisible(template)
}

#' Load a CSV Template from an XML File
#'
#' Reads an XML file written by \code{\link{save_csv_template}()} and returns
#' a \code{csv_template} object. The returned object can be passed directly to
#' \code{\link{initiate}()} via its \code{template} argument.
#'
#' @param path Character; path to the XML file.
#'
#' @return A \code{csv_template} object.
#' @seealso \code{\link{csv_template}}, \code{\link{save_csv_template}}
#' @export
load_csv_template <- function(path) {
  if (!file.exists(path))
    stop("load_csv_template(): file not found: ", path)

  content <- paste(readLines(path, warn = FALSE), collapse = "\n")

  get_tag <- function(tag, text) {
    pattern <- sprintf("<%s>([^<]*)</%s>", tag, tag)
    m <- regmatches(text, regexpr(pattern, text, perl = TRUE))
    if (length(m) == 0 || identical(m, character(0))) return(NULL)
    val <- sub(sprintf("^<%s>", tag), "", sub(sprintf("</%s>$", tag), "", m))
    val <- .xml_unescape(val)
    if (nchar(val) == 0) NULL else val
  }

  # Extract <columns> sub-section ((?s) = DOTALL, . matches newlines)
  cols_m <- regexpr("(?s)<columns>(.*?)</columns>", content, perl = TRUE)
  cols_section <- if (cols_m > 0) regmatches(content, cols_m) else content

  # Extract <extra_columns>
  col_extra <- NULL
  extra_m <- regexpr("(?s)<extra_columns>(.*?)</extra_columns>", content, perl = TRUE)
  if (extra_m > 0) {
    extra_section <- regmatches(content, extra_m)

    # New format: <col ref="..." as="..."/>
    ref_matches <- gregexpr('<col ref="[^"]*"[^/]*/>', extra_section, perl = TRUE)
    ref_vals <- regmatches(extra_section, ref_matches)[[1]]

    if (length(ref_vals) > 0) {
      # Extract ref attribute value via backreference
      refs <- .xml_unescape(gsub('^.*<col ref="([^"]*)".*$', "\\1", ref_vals))

      as_vals <- vapply(ref_vals, function(tag) {
        if (!grepl('as="', tag, fixed = TRUE)) return(NA_character_)
        .xml_unescape(gsub('^.*as="([^"]*)".*$', "\\1", tag))
      }, character(1))

      parsed_refs <- lapply(refs, .parse_col_ref)

      # Rebuild col_extra: named if any 'as' is present, else plain
      has_names <- any(!is.na(as_vals))
      out_names <- ifelse(is.na(as_vals), "", as_vals)

      # Detect if all refs are integer
      all_int <- all(vapply(parsed_refs, is.integer, logical(1)))
      col_extra_vec <- if (all_int)
        vapply(parsed_refs, as.integer, integer(1))
      else
        vapply(parsed_refs, function(x) as.character(x), character(1))

      if (has_names) names(col_extra_vec) <- out_names
      col_extra <- col_extra_vec

    } else {
      # Legacy format: <col>value</col>
      old_matches <- gregexpr("<col>([^<]*)</col>", extra_section, perl = TRUE)
      old_vals <- regmatches(extra_section, old_matches)[[1]]
      if (length(old_vals) > 0) {
        col_extra <- .xml_unescape(sub("^<col>", "", sub("</col>$", "", old_vals)))
      }
    }
  }

  coord_system <- get_tag("coord_system",    content) %||% "gps"
  skip <- as.integer(get_tag("skip",            content) %||% "0")
  max_empty_lines <- as.integer(get_tag("max_empty_lines", content) %||% "3")
  comment <- get_tag("comment",         content) %||% "#"
  duplicate_method <- get_tag("duplicate_method", content) %||% "return_error"
  delimiter <- get_tag("delimiter", content) %||% ","
  decimal   <- get_tag("decimal",   content) %||% "."

  # col_* values may be integer indices or column names
  col_unix <- .parse_col_ref(get_tag("col_unix",         cols_section))
  col_coord1 <- .parse_col_ref(get_tag("col_coord1",       cols_section))
  col_coord2 <- .parse_col_ref(get_tag("col_coord2",       cols_section))
  col_coord3 <- .parse_col_ref(get_tag("col_coord3",       cols_section))
  col_velocity <- .parse_col_ref(get_tag("col_velocity",     cols_section))
  col_acceleration <- .parse_col_ref(get_tag("col_acceleration", cols_section))

  if (is.null(col_unix))
    stop("load_csv_template(): <col_unix> not found in XML")
  if (is.null(col_coord1))
    stop("load_csv_template(): <col_coord1> (lat / x) not found in XML")
  if (is.null(col_coord2))
    stop("load_csv_template(): <col_coord2> (lng / y) not found in XML")

  structure(
    list(
      coord_system = coord_system,
      skip = skip,
      max_empty_lines = max_empty_lines,
      comment = comment,
      duplicate_method = duplicate_method,
      delimiter = delimiter,
      decimal = decimal,
      col_unix = col_unix,
      col_coord1 = col_coord1,
      col_coord2 = col_coord2,
      col_coord3 = col_coord3,
      col_velocity = col_velocity,
      col_acceleration = col_acceleration,
      col_extra = col_extra
    ),
    class = "csv_template"
  )
}

#' Guess a CSV Template from an Example File
#'
#' Runs the same column-detection logic as \code{source = 'guess_csv'} in
#' \code{\link{initiate}()} and converts the detected schema into a locked
#' \code{\link{csv_template}} object.
#'
#' \strong{Recommended workflow:}
#' \enumerate{
#'   \item Call \code{guess_csv_template(path, save_path = "my_device.xml")}
#'     once on a representative file to capture the resolved column mapping.
#'   \item Inspect the returned template and adjust with
#'     \code{\link{csv_template}()} if any columns were mis-detected.
#'   \item Load the saved template in all subsequent scripts with
#'     \code{initiate(template = "my_device.xml")} — column parsing is then
#'     explicit, deterministic, and independent of column-name heuristics.
#' }
#'
#' @param path Character; path to an example CSV file to inspect.
#' @param save_path Character or \code{NULL}; if supplied the template is
#'   saved to this path via \code{\link{save_csv_template}()}.
#' @param coord_system Character; \code{"auto"} (default), \code{"gps"}, or
#'   \code{"local"}.  Passed to the underlying guesser.
#'
#' @return A \code{csv_template} object.  Returned invisibly when
#'   \code{save_path} is supplied, visibly otherwise.
#' @seealso \code{\link{csv_template}}, \code{\link{save_csv_template}},
#'   \code{\link{load_csv_template}}, \code{\link{initiate}}
#' @export
guess_csv_template <- function(path, save_path = NULL, coord_system = "auto") {
  if (!file.exists(path))
    stop("guess_csv_template(): file not found: ", path)

  coord_system <- match.arg(coord_system, c("auto", "gps", "local"))

  trace <- initiate_guess_csv(path, source = coord_system)

  mapping <- attr(trace, "column_mapping")
  skip_n  <- attr(trace, "skip") %||% 0L
  cs      <- attr(trace, "coord_system_detected") %||% "gps"

  # Read the raw header row so we can detect duplicate column names and replace
  # them with integer indices (which are unambiguous).
  raw_lines  <- readLines(path, n = skip_n + 1L, warn = FALSE)
  raw_header <- raw_lines[skip_n + 1L]
  raw_cols   <- trimws(strsplit(raw_header, ",")[[1]])
  raw_cols   <- raw_cols[nchar(raw_cols) > 0]
  raw_lower  <- tolower(raw_cols)

  # Return the 1-based integer index of `name` when it appears more than once
  # in the raw header; otherwise return the name as-is (cleaner template output).
  safe_ref <- function(name) {
    if (is.null(name)) return(NULL)
    hits <- which(raw_lower == tolower(name))
    if (length(hits) > 1L) hits[1L] else name
  }

  col_unix <- safe_ref(mapping$unix_time)
  if (is.null(col_unix))
    stop("guess_csv_template(): could not detect a unix_time column — ",
         "specify column names manually with csv_template()")

  if (cs == "gps") {
    if (is.null(mapping$lat) || is.null(mapping$lng))
      stop("guess_csv_template(): could not detect lat/lng columns — ",
           "specify column names manually with csv_template()")
    tmpl <- csv_template(
      coord_system = "gps",
      col_unix     = col_unix,
      col_lat      = safe_ref(mapping$lat),
      col_lng      = safe_ref(mapping$lng),
      col_altitude = safe_ref(mapping$altitude),
      skip         = skip_n
    )
  } else {
    if (is.null(mapping$x) || is.null(mapping$y))
      stop("guess_csv_template(): could not detect x/y columns — ",
           "specify column names manually with csv_template()")
    tmpl <- csv_template(
      coord_system = "local",
      col_unix     = col_unix,
      col_x        = safe_ref(mapping$x),
      col_y        = safe_ref(mapping$y),
      col_z        = safe_ref(mapping$z),
      skip         = skip_n
    )
  }

  if (!is.null(save_path)) {
    save_csv_template(tmpl, save_path)
    message("Template saved to: ", save_path)
    return(invisible(tmpl))
  }

  tmpl
}

# ── Internal CSV reader ────────────────────────────────────────────────────────

# Resolve a column reference (string name or 1-based integer) to a column index.
# Returns an integer index into all_cols.
#' @keywords internal
.find_col_ref <- function(ref, all_cols, col_lower, duplicate_method) {
  if (is.null(ref)) return(NULL)
  n <- length(all_cols)

  if (is.numeric(ref) || is.integer(ref)) {
    idx <- as.integer(ref)
    if (idx < 1L || idx > n)
      stop(sprintf(
        "csv_template: column index %d is out of range (file has %d columns)",
        idx, n
      ))
    return(idx)
  }

  # String: case-insensitive match
  matches <- which(col_lower == stringr::str_to_lower(as.character(ref)))

  if (length(matches) == 0)
    stop(sprintf(
      "csv_template: column '%s' not found. Available: %s",
      ref, paste(all_cols, collapse = ", ")
    ))

  if (length(matches) > 1) {
    dup_info <- paste(matches, collapse = ", ")
    if (duplicate_method == "return_error")
      stop(sprintf(
        "csv_template: column name '%s' appears %d times (positions: %s). Use an integer index to select a specific column, or set duplicate_method = 'use_first' / 'use_second'.",
        ref, length(matches), dup_info
      ))
    else if (duplicate_method == "use_second") {
      if (length(matches) < 2)
        stop(sprintf(
          "csv_template: duplicate_method = 'use_second' but '%s' only appears once",
          ref
        ))
      return(matches[2])
    }
    # use_first falls through to matches[1]
  }

  matches[1]
}

#' Read a CSV file using a csv_template specification
#' @keywords internal
initiate_template_csv <- function(.data_path, template) {

  all_lines <- readr::read_lines(.data_path)

  # Skip preamble rows
  if (template$skip > 0)
    all_lines <- all_lines[(template$skip + 1L):length(all_lines)]

  # Strip comment lines
  if (nchar(template$comment) > 0)
    all_lines <- all_lines[!stringr::str_starts(
      stringr::str_trim(all_lines, side = "left"),
      stringr::fixed(template$comment)
    )]

  # Detect data end: stop at max_empty_lines consecutive blank rows
  empty_flags <- stringr::str_detect(all_lines, "^\\s*$")
  consecutive_empty <- 0L
  stop_at <- length(all_lines)

  for (i in seq_along(all_lines)) {
    if (empty_flags[i]) {
      consecutive_empty <- consecutive_empty + 1L
      if (consecutive_empty >= template$max_empty_lines) {
        stop_at <- i - template$max_empty_lines
        break
      }
    } else {
      consecutive_empty <- 0L
    }
  }

  data_lines <- all_lines[1:stop_at]
  data_lines <- data_lines[!empty_flags[1:stop_at]]

  if (length(data_lines) < 2)
    stop("csv_template: no data rows found after skip and blank-row trimming")

  delim <- template$delimiter %||% ","
  dec   <- template$decimal   %||% "."

  df <- suppressWarnings(
    readr::read_delim(
      I(paste(data_lines, collapse = "\n")),
      delim = delim,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character()),
      name_repair = "minimal"   # preserve duplicate column names for duplicate_method
    )
  )

  if (nrow(df) == 0)
    stop("csv_template: no data rows found after parsing")

  # Helper: substitute decimal separator then coerce to numeric
  parse_num <- function(x) {
    if (dec != ".") x <- gsub(dec, ".", x, fixed = TRUE)
    suppressWarnings(as.numeric(x))
  }

  all_cols <- colnames(df)
  col_lower <- stringr::str_to_lower(all_cols)
  dm <- template$duplicate_method

  find <- function(ref) .find_col_ref(ref, all_cols, col_lower, dm)

  unix_idx <- find(template$col_unix)
  coord1_idx <- find(template$col_coord1)
  coord2_idx <- find(template$col_coord2)
  coord3_idx <- find(template$col_coord3)   # NULL if not specified
  vel_idx <- find(template$col_velocity)
  acc_idx <- find(template$col_acceleration)

  # Resolve col_extra: named/unnamed character or integer vector
  extra_resolved <- list()  # list of list(idx, out_name)
  if (!is.null(template$col_extra)) {
    nms <- names(template$col_extra)
    for (i in seq_along(template$col_extra)) {
      ref <- template$col_extra[[i]]
      usr_nm <- if (!is.null(nms) && nchar(nms[i]) > 0) nms[i] else NULL

      idx <- tryCatch(find(ref), error = function(e) {
        warning(sprintf("csv_template: %s — skipping.", conditionMessage(e)))
        NULL
      })
      if (is.null(idx)) next

      # Determine output column name
      out_nm <- if (!is.null(usr_nm)) {
        usr_nm
      } else if (is.numeric(ref) || is.integer(ref)) {
        sprintf("col_%d", as.integer(ref))
      } else {
        all_cols[idx]  # preserve actual CSV case
      }

      extra_resolved <- c(extra_resolved, list(list(idx = idx, name = out_nm)))
    }
  }

  # Standard output column names
  std_names <- if (template$coord_system == "gps")
    c("unix_time", "lat", "lng", "altitude")
  else
    c("unix_time", "x", "y", "z")

  # Pre-process unix column for non-standard decimal before convert_to_unix
  unix_raw <- df[[unix_idx]]
  if (dec != ".") unix_raw <- gsub(dec, ".", unix_raw, fixed = TRUE)

  # Build output tibble with standard columns (access by integer index)
  output <- tibble::tibble(
    unix_time = convert_to_unix(unix_raw),
    coord1 = parse_num(df[[coord1_idx]]),
    coord2 = parse_num(df[[coord2_idx]]),
    coord3 = if (!is.null(coord3_idx)) parse_num(df[[coord3_idx]]) else NA_real_
  )
  colnames(output) <- std_names

  # Pre-computed velocity / acceleration
  if (!is.null(vel_idx))
    output[["velocity"]] <- parse_num(df[[vel_idx]])

  if (!is.null(acc_idx))
    output[["acceleration"]] <- parse_num(df[[acc_idx]])

  # Extra columns: check for reserved column collisions, then import
  if (length(extra_resolved) > 0) {
    .check_extra_collision(vapply(extra_resolved, `[[`, character(1), "name"), "csv")
    for (er in extra_resolved) {
      raw <- df[[er$idx]]
      num_attempt <- parse_num(raw)
      # Numeric only if every non-NA value converted cleanly; otherwise character.
      # Strict type prevents double↔character mismatch when binding rows across files.
      output[[er$name]] <- if (all(!is.na(num_attempt) | is.na(raw))) num_attempt else as.character(raw)
    }
  }

  # NA diagnostics
  n_na_unix <- sum(is.na(output[["unix_time"]]))
  if (n_na_unix > 0)
    warning(sprintf("csv_template: %d unix_time value(s) are NA", n_na_unix))

  # Column mapping for metadata (records original references)
  mapping <- list()
  mapping[["unix_time"]]  <- template$col_unix
  mapping[[std_names[2]]] <- template$col_coord1
  mapping[[std_names[3]]] <- template$col_coord2
  mapping[[std_names[4]]] <- template$col_coord3 %||% "[not specified]"
  if (!is.null(template$col_velocity))
    mapping[["velocity"]]     <- template$col_velocity
  if (!is.null(template$col_acceleration))
    mapping[["acceleration"]] <- template$col_acceleration
  if (length(extra_resolved) > 0)
    mapping[["extra_columns"]] <- vapply(extra_resolved, `[[`, character(1), "name")

  attr(output, "column_mapping") <- mapping
  output
}


