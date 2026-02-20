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
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer(file_id, "file_id")
  assert_positive_integer(record_id, "record_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
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
