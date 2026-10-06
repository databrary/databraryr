#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Replace a Session in Databrary Volume (PUT)
#'
#' @description Sends a PUT request to fully replace a session's writable
#' fields. \code{name} is required (non-empty); other fields are optional and
#' default to server-side values when omitted (the underlying serializer
#' marks them \code{required=FALSE}). Use \code{\link{patch_session}} for
#' partial updates when you don't want full-replacement semantics.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param name New session name. Required, non-empty after trim.
#' @param release_level Optional release level (e.g. \code{"PRIVATE"},
#'   \code{"SHARED"}, \code{"EXCERPTS"}, \code{"PUBLIC"}).
#' @param source_date Optional session date. A length-1 \code{Date} object or
#'   ISO \code{"YYYY-MM-DD"} string. Mutually exclusive with \code{date}.
#' @param date Optional structured date list with named fields \code{year},
#'   \code{month}, \code{day}, and optional \code{is_estimated} (logical).
#'   Mutually exclusive with \code{source_date}.
#' @param date_precision Optional precision for \code{date}: e.g.
#'   \code{"FULL"}, \code{"YEAR"}.
#' @param default_records Optional integer vector of record IDs. The server
#'   replaces the session's current default records with this set; omit to
#'   leave them unchanged.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list with the updated session's metadata (same shape as
#'   \code{\link{get_session_by_id}}), or \code{NULL} if the update fails.
#'
#' @seealso \code{\link{patch_session}}, \code{\link{create_session}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Rename a session via PUT
#' update_session(vol_id = 1, session_id = 42, name = "Replacement name")
#' }
#' }
#' @export
update_session <- function(
  vol_id = 1,
  session_id,
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
  assert_positive_integer(session_id, "session_id")

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
  if (!is.null(release_level)) body$release_level <- release_level
  if (!is.null(source_date)) body$source_date <- coerce_iso_date(source_date, "source_date")
  if (!is.null(date)) body$date <- date
  if (!is.null(date_precision)) body$date_precision <- date_precision
  if (!is.null(default_records)) {
    body$default_records <- as.list(as.integer(default_records))
  }

  session <- perform_api_put(
    path = sprintf(API_SESSION_DETAIL, vol_id, session_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(session)) {
    if (vb) {
      message(
        "Failed to replace session ", session_id, " in volume ", vol_id
      )
    }
    return(NULL)
  }

  session
}
