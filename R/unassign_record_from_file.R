#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Unassign Record from Session File
#'
#' @description Remove the association between a record and a session file.
#' If the record is not assigned to the file, this operation will fail.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param file_id Numeric file identifier. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the record was successfully unassigned, \code{FALSE}
#'   otherwise.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Unassign a record from a file
#' unassign_record_from_file(
#'   vol_id = 1,
#'   session_id = 10,
#'   file_id = 20,
#'   record_id = 123
#' )
#' }
#' }
#' @export
unassign_record_from_file <- function(
  vol_id = 1,
  session_id,
  file_id,
  record_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  # Validate vol_id
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  assertthat::assert_that(
    vol_id == floor(vol_id),
    msg = "vol_id must be an integer"
  )

  # Validate session_id
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(session_id > 0)
  assertthat::assert_that(
    session_id == floor(session_id),
    msg = "session_id must be an integer"
  )

  # Validate file_id
  assertthat::assert_that(is.numeric(file_id))
  assertthat::assert_that(length(file_id) == 1)
  assertthat::assert_that(file_id > 0)
  assertthat::assert_that(
    file_id == floor(file_id),
    msg = "file_id must be an integer"
  )

  # Validate record_id
  assertthat::assert_that(is.numeric(record_id))
  assertthat::assert_that(length(record_id) == 1)
  assertthat::assert_that(record_id > 0)
  assertthat::assert_that(
    record_id == floor(record_id),
    msg = "record_id must be an integer"
  )

  # Validate vb
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  # Validate rq
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Build request body
  body <- list(record_id = record_id)

  # Perform API call (unassign returns 204 No Content)
  success <- perform_api_post(
    path = sprintf(API_SESSION_FILE_UNASSIGN, vol_id, session_id, file_id),
    body = body,
    rq = rq,
    vb = vb
  )
  if (is.null(success)) success <- FALSE

  if (!success && vb) {
    message(
      "Failed to unassign record ",
      record_id,
      " from file ",
      file_id,
      " in session ",
      session_id,
      " of volume ",
      vol_id
    )
  }

  success
}
