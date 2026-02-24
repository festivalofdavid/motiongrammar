# segment name: utils_quality ---
# Internal quality-log helpers used across the entire pipeline.
# Kept in a dedicated file so every pipeline verb can rely on them
# without depending on load order.

#' Get the installed motiongrammar package version
#'
#' Uses utils::packageName() so the exact installed name is always used.
#' Falls back to "dev" when the package is loaded in development mode or
#' the version cannot be determined.
#' @keywords internal
.mg_pkg_version <- function() {
  tryCatch({
    pkg <- utils::packageName()
    if (is.null(pkg) || !nzchar(pkg)) return("dev")
    as.character(utils::packageVersion(pkg))
  }, error = function(e) "dev")
}

#' Generate a unique step identifier for quality log entries
#'
#' Uses uuid::UUIDgenerate() when available; falls back to a timestamp-based
#' string if the uuid package is not installed.
#' @keywords internal
.mg_step_id <- function() {
  tryCatch(
    uuid::UUIDgenerate(),
    error = function(e) paste0("step-", format(Sys.time(), "%Y%m%d%H%M%OS3"))
  )
}

#' Return current time as a UTC ISO 8601 string
#' @keywords internal
.mg_timestamp <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%OS3Z")
}

#' Format a timestamp for display
#'
#' Handles both \code{POSIXt} objects (e.g. \code{meta$created_timestamp})
#' and ISO 8601 character strings produced by \code{.mg_timestamp()}.
#' @keywords internal
.mg_fmt_ts <- function(ts) {
  if (is.null(ts) || (length(ts) == 1L && is.na(ts))) return("N/A")
  if (inherits(ts, "POSIXt")) return(format(ts, "%Y-%m-%d %H:%M:%S"))
  # Character: "2026-02-23T14:30:00.123Z" -> "2026-02-23 14:30:00"
  sub("T", " ", substr(as.character(ts), 1L, 19L))
}
