#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Assign Record to Session File
#'
#' @description Assign a record to a session file, creating a record-file
#' association. This operation is idempotent - calling it multiple times with
#' the same parameters will not create duplicate associations.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param file_id Numeric file identifier. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer and
#'   must belong to the specified volume.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return The response data on success, or \code{NULL} if the operation fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Assign a participant record to a video file
#' assign_record_to_file(
#'   vol_id = 1,
#'   session_id = 10,
#'   file_id = 20,
#'   record_id = 123
#' )
#' }
#' }
#' @export
assign_record_to_file <- function(
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

  # Perform API call
  result <- perform_api_post(
    path = sprintf(API_SESSION_FILE_ASSIGN, vol_id, session_id, file_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(result)) {
    if (vb) {
      message(
        "Failed to assign record ",
        record_id,
        " to file ",
        file_id,
        " in session ",
        session_id,
        " of volume ",
        vol_id
      )
    }
    return(NULL)
  }

  result
}
