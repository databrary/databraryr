#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Add a Default Record to a Session
#'
#' @description Attach a record to a session as a default record. Default
#' records apply to all files in the session unless overridden. The record
#' must either belong to the destination volume or be accessible to it via a
#' linked volume; the server enforces this and returns \code{403} otherwise.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the record was successfully added, \code{FALSE}
#'   otherwise.
#'
#' @seealso \code{\link{remove_default_record_from_session}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' add_default_record_to_session(vol_id = 1, session_id = 42, record_id = 101)
#' }
#' }
#' @export
add_default_record_to_session <- function(
  vol_id = 1,
  session_id,
  record_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer(record_id, "record_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  result <- perform_api_post(
    path = sprintf(API_SESSION_ADD_DEFAULT_RECORD, vol_id, session_id),
    body = list(record_id = record_id),
    rq = rq,
    vb = vb
  )

  if (is.null(result)) {
    if (vb) {
      message(
        "Failed to add default record ", record_id,
        " to session ", session_id, " in volume ", vol_id
      )
    }
    return(FALSE)
  }

  TRUE
}
