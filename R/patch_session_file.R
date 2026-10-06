#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Partially Update a Session File's Metadata
#'
#' @description Sends a PATCH request to update selected metadata fields of an
#' existing session file. Only provided arguments are sent; omitted fields are
#' left unchanged on the server.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param file_id Numeric file identifier. Must be a positive integer.
#' @param name Optional new file name. If provided, must be a non-empty
#'   length-1 string.
#' @param release_level Optional release level (e.g. \code{"PRIVATE"},
#'   \code{"SHARED"}, \code{"EXCERPTS"}, \code{"PUBLIC"}). Server validates
#'   the choice.
#' @param source_date Optional file date. A length-1 \code{Date} object or
#'   ISO \code{"YYYY-MM-DD"} string. Mutually exclusive with \code{date}.
#' @param date Optional structured date list with named fields \code{year},
#'   \code{month}, \code{day}, and optional \code{is_estimated} (logical).
#'   Mutually exclusive with \code{source_date}.
#' @param date_precision Optional precision for \code{date}: e.g.
#'   \code{"FULL"}, \code{"YEAR"}.
#' @param is_estimated Optional logical flag indicating whether the date is
#'   estimated.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list with the updated file's metadata (same shape as
#'   \code{\link{get_session_file}}), or \code{NULL} if the update fails or
#'   no fields were provided.
#'
#' @seealso \code{\link{update_session_file}}, \code{\link{delete_session_file}},
#'   \code{\link{get_session_file}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Rename a file
#' patch_session_file(
#'   vol_id = 1, session_id = 42, file_id = 99, name = "Renamed.mp4"
#' )
#'
#' # Update the date and precision
#' patch_session_file(
#'   vol_id = 1,
#'   session_id = 42,
#'   file_id = 99,
#'   date = list(year = 2024, month = 3, day = 15),
#'   date_precision = "FULL"
#' )
#' }
#' }
#' @export
patch_session_file <- function(
  vol_id = 1,
  session_id,
  file_id,
  name = NULL,
  release_level = NULL,
  source_date = NULL,
  date = NULL,
  date_precision = NULL,
  is_estimated = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer(file_id, "file_id")

  if (!is.null(name)) {
    assertthat::assert_that(is.character(name), length(name) == 1)
    assertthat::assert_that(
      nzchar(trimws(name)),
      msg = "name must not be empty"
    )
  }

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

  if (!is.null(is_estimated)) {
    assertthat::assert_that(is.logical(is_estimated), length(is_estimated) == 1)
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  body <- list()
  if (!is.null(name)) body$name <- name
  if (!is.null(release_level)) body$release_level <- release_level
  if (!is.null(source_date)) {
    body$source_date <- coerce_iso_date(source_date, "source_date")
  }
  if (!is.null(date)) body$date <- date
  if (!is.null(date_precision)) body$date_precision <- date_precision
  if (!is.null(is_estimated)) body$is_estimated <- is_estimated

  if (length(body) == 0) {
    if (vb) {
      message(
        "No fields provided to update for file ", file_id,
        " in session ", session_id
      )
    }
    return(NULL)
  }

  file <- perform_api_patch(
    path = sprintf(API_SESSION_FILE_DETAIL, vol_id, session_id, file_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(file)) {
    if (vb) {
      message(
        "Failed to update file ", file_id,
        " in session ", session_id,
        " of volume ", vol_id
      )
    }
    return(NULL)
  }

  file
}
