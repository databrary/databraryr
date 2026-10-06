#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Coerce a date-like argument to an ISO "YYYY-MM-DD" string.
#'
#' Accepts a `Date` object or a length-1 character already in ISO form.
#'
#' @noRd
coerce_iso_date <- function(value, name) {
  if (inherits(value, "Date")) {
    assertthat::assert_that(
      length(value) == 1,
      msg = paste(name, "must have length 1")
    )
    return(format(value, "%Y-%m-%d"))
  }
  assertthat::assert_that(
    is.character(value),
    length(value) == 1,
    nzchar(trimws(value)),
    msg = paste(name, "must be a Date or non-empty 'YYYY-MM-DD' string")
  )
  parsed <- tryCatch(as.Date(value), error = function(e) NA)
  assertthat::assert_that(
    !is.na(parsed),
    msg = paste(name, "must parse as a date (e.g. '2024-03-15')")
  )
  format(parsed, "%Y-%m-%d")
}

#' Create Session in Databrary Volume
#'
#' @description Create a new session in a Databrary volume. A session (a.k.a.
#' "slot") groups files and metadata for a single recording or testing event.
#' \code{name} is required and must be non-empty. Provide either a flat
#' \code{source_date} or a structured \code{date} list (with optional
#' \code{date_precision}) to record when the session occurred -- not both.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param name Display name for the session. Required, non-empty after trim.
#' @param release_level Optional release level for the session
#'   (e.g. \code{"PRIVATE"}, \code{"SHARED"}, \code{"EXCERPTS"},
#'   \code{"PUBLIC"}). The server validates the choice.
#' @param source_date Optional session date. A length-1 \code{Date} object or
#'   ISO \code{"YYYY-MM-DD"} string. Mutually exclusive with \code{date}.
#' @param date Optional structured date list with named fields \code{year},
#'   \code{month}, \code{day}, and optional \code{is_estimated} (logical).
#'   Mutually exclusive with \code{source_date}.
#' @param date_precision Optional precision for \code{date}: e.g.
#'   \code{"FULL"}, \code{"YEAR"}. Server validates the choice.
#' @param default_records Optional integer vector of record IDs to set as
#'   default records on the new session.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list with the created session's metadata (same shape as
#'   \code{\link{get_session_by_id}}), or \code{NULL} if creation fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Minimal session
#' create_session(vol_id = 1, name = "Pilot 01")
#'
#' # Session with a flat date
#' create_session(
#'   vol_id = 1,
#'   name = "Pilot 02",
#'   source_date = as.Date("2024-03-15")
#' )
#'
#' # Session with a structured date and default records
#' create_session(
#'   vol_id = 1,
#'   name = "Pilot 03",
#'   date = list(year = 2024, month = 3, day = 15, is_estimated = FALSE),
#'   date_precision = "FULL",
#'   default_records = c(101, 102)
#' )
#' }
#' }
#' @export
create_session <- function(
  vol_id = 1,
  name,
  release_level = NULL,
  source_date = NULL,
  date = NULL,
  date_precision = NULL,
  default_records = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")

  assertthat::assert_that(is.character(name), length(name) == 1)
  assertthat::assert_that(nzchar(trimws(name)), msg = "name must not be empty")

  if (!is.null(release_level)) {
    assertthat::assert_that(
      is.character(release_level),
      length(release_level) == 1,
      nzchar(trimws(release_level))
    )
  }

  assertthat::assert_that(
    is.null(source_date) || is.null(date),
    msg = "Provide either source_date or date, not both."
  )

  if (!is.null(date)) {
    assertthat::assert_that(
      is.list(date),
      !is.null(names(date)),
      msg = "date must be a named list (e.g. list(year=, month=, day=))"
    )
  }

  if (!is.null(date_precision)) {
    assertthat::assert_that(
      is.character(date_precision),
      length(date_precision) == 1,
      nzchar(trimws(date_precision))
    )
  }

  if (!is.null(default_records)) {
    assertthat::assert_that(
      is.numeric(default_records),
      length(default_records) >= 1,
      all(default_records >= 1),
      all(default_records == floor(default_records)),
      msg = "default_records must be a vector of positive integers"
    )
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  body <- list(name = name)

  if (!is.null(release_level)) {
    body$release_level <- release_level
  }
  if (!is.null(source_date)) {
    body$source_date <- coerce_iso_date(source_date, "source_date")
  }
  if (!is.null(date)) {
    body$date <- date
  }
  if (!is.null(date_precision)) {
    body$date_precision <- date_precision
  }
  if (!is.null(default_records)) {
    body$default_records <- as.list(as.integer(default_records))
  }

  session <- perform_api_post(
    path = sprintf(API_VOLUME_SESSIONS, vol_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(session)) {
    if (vb) {
      message("Failed to create session in volume ", vol_id)
    }
    return(NULL)
  }

  session
}
