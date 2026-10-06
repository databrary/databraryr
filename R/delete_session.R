#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Delete Session from Databrary Volume
#'
#' @description Delete (soft-delete) a session from a Databrary volume. The
#' session and its associated metadata are marked as deleted but not
#' permanently removed from the database.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the session was successfully deleted, \code{FALSE}
#'   otherwise.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Delete a session
#' delete_session(vol_id = 1, session_id = 42)
#'
#' # Delete with verbose output
#' delete_session(vol_id = 1, session_id = 42, vb = TRUE)
#' }
#' }
#' @export
delete_session <- function(
  vol_id = 1,
  session_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  success <- perform_api_delete(
    path = sprintf(API_SESSION_DETAIL, vol_id, session_id),
    rq = rq,
    vb = vb
  )

  if (!success) {
    if (vb) {
      message(
        "Failed to delete session ",
        session_id,
        " from volume ",
        vol_id
      )
    }
  }

  success
}
