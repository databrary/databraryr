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
