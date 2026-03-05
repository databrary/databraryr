#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Request a Session or Volume CSV Export.
#'
#' @description
#' The Django API generates CSV reports asynchronously. This function queues a
#' CSV export for a specific session when `session_id` is supplied, or for the
#' entire volume when `session_id` is `NULL`. The API delivers the final signed
#' download link via email once the export is ready.
#'
#' @param vol_id Integer. Target volume identifier. Default is 2.
#' @param session_id Optional integer. When provided, requests a session-level
#'   CSV export. When `NULL`, a volume-level CSV export is requested. Default is
#'   9.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is `NULL`, meaning a default
#'   authenticated request is generated.
#'
#' @returns A list describing the processing task (`status`, `message`,
#'   `task_id`) or `NULL` if the request fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Request a volume-wide CSV export
#' download_session_csv() # CSV for default volume 2
#'
#' # Request a session-specific CSV export
#' download_session_csv(vol_id = 2, session_id = 9)
#' }
#' }
#'
#' @export
download_session_csv <- function(vol_id = 1,
                                 session_id = NULL,
                                 vb = options::opt("vb"),
                                 rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  if (!is.null(session_id)) {
    assertthat::assert_that(length(session_id) == 1)
    assertthat::assert_that(is.numeric(session_id))
    assertthat::assert_that(session_id >= 1)
  }
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) ||
                            ("httr2_request" %in% class(rq)))
  
  path <- if (is.null(session_id)) {
    sprintf(API_VOLUME_CSV_DOWNLOAD_LINK, vol_id)
  } else {
    sprintf(API_SESSION_CSV_DOWNLOAD_LINK, vol_id, session_id)
  }
  
  request_processing_task(path = path, rq = rq, vb = vb)
}
