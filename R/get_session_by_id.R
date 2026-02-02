#' @eval options::as_params()
#' @name options_params
#' 
NULL

#' Get Session (Slot) Data From A Databrary Volume
#'
#' @param session_id An integer indicating a valid session/slot identifier
#' linked to a volume. Default value is 9807, the materials folder for volume 1.
#' @param vol_id An integer indicating the volume identifier. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An httr2 request object.
#'
#' @returns A JSON blob with the session data. If the user has previously logged
#' in to Databrary via `login_db()`, then session(s) that have restricted access
#' can be downloaded, subject to the sharing release levels on those session(s).
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' get_session_by_id() # Default is Volume 1, Session 9807, the Materials folder
#' }
#' }
#' @export
get_session_by_id <-
  function(session_id = 9807,
           vol_id = 1,
           vb = options::opt("vb"),
           rq = NULL) {

    assertthat::assert_that(is.numeric(session_id))
    assertthat::assert_that(session_id > 0)
    assertthat::assert_that(length(session_id) == 1)

    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id > 0)
    assertthat::assert_that(length(vol_id) == 1)

    assertthat::assert_that(is.logical(vb))
    assertthat::assert_that(length(vb) == 1)

    assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

    session <- perform_api_get(
      path = sprintf(API_SESSION_DETAIL, vol_id, session_id),
      rq = rq,
      vb = vb
    )

    if (is.null(session)) {
      if (vb) {
        message("Cannot access requested session ", session_id, " in volume ", vol_id)
      }
      return(NULL)
    }

    session
  }
