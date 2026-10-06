#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Remove a Default Record from a Session
#'
#' @description Detach a record from a session's default records. The record
#' must currently be a default record on the session, otherwise the server
#' returns \code{404}.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the record was successfully removed, \code{FALSE}
#'   otherwise.
#'
#' @seealso \code{\link{add_default_record_to_session}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' remove_default_record_from_session(vol_id = 1, session_id = 42, record_id = 101)
#' }
#' }
#' @export
remove_default_record_from_session <- function( # nolint: object_length_linter.
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
    path = sprintf(API_SESSION_REMOVE_DEFAULT_RECORD, vol_id, session_id),
    body = list(record_id = record_id),
    rq = rq,
    vb = vb
  )

  if (is.null(result)) {
    if (vb) {
      message(
        "Failed to remove default record ", record_id,
        " from session ", session_id, " in volume ", vol_id
      )
    }
    return(FALSE)
  }

  TRUE
}
