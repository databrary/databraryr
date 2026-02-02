#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Request a Signed ZIP Download for a Session.
#'
#' @description
#' The Django API prepares session-level ZIP archives asynchronously. Calling
#' `download_session_zip()` triggers the job and returns a processing task
#' summary. Once the archive is ready, Databrary emails a signed download link
#' to the authenticated user.
#'
#' @param vol_id Volume identifier that owns the session.
#' @param session_id Session identifier within the volume.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is `NULL`, in which case a
#'   default authenticated request is generated.
#'
#' @returns A list describing the processing task (`status`, `message`,
#'   `task_id`) or `NULL` when the request fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' download_session_zip(vol_id = 31, session_id = 9803)
#' }
#' }
#'
#' @export
download_session_zip <- function(vol_id = 31,
                                 session_id = 9803,
                                 vb = options::opt("vb"),
                                 rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) ||
                            ("httr2_request" %in% class(rq)))
  
  path <- sprintf(API_SESSION_DOWNLOAD_LINK, vol_id, session_id)
  request_processing_task(path = path, rq = rq, vb = vb)
}
