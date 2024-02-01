#' Get Session (Slot) Data From A Databrary Volume
#'
#' @param session_id An integer indicating a valid session/slot identifier
#' linked to a volume. Default value is 9807, the materials folder for volume 1.
#' @param volume_json A JSON blob with data from a Databrary volume such as that
#' returned by `get_volume_by_id()`. If no JSON blob is supplied, the blob
#' returned by `get_volume_by_id()` is used.
#' @param vb A logical value. Show verbose feedback. Default is FALSE.
#' @param rq An httr2 request object.
#'
#' @returns A JSON blob with the volume data. If the user has previously logged
#' in to Databrary via `login_db()`, then volume(s) that have restricted access
#' can be downloaded, subject to the sharing release levels on those volume(s).
#'
#' @examples
#' \donttest{
#' get_session_by_id() # Default is Volume 1, Session 9807, the Materials folder
#' }
#' @export
get_session_by_id <-
  function(session_id = 9807,
           volume_json = get_volume_by_id(vol_id = 1, rq),
           vb = FALSE,
           rq = NULL) {
    assertthat::assert_that(is.numeric(session_id))
    assertthat::assert_that(session_id > 0)
    assertthat::assert_that(length(session_id) == 1)
    
    assertthat::assert_that(is.list(volume_json))
    
    assertthat::assert_that(is.logical(vb))
    assertthat::assert_that(length(vb) == 1)
    
    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))
    
    # Handle NULL rq
    if (is.null(rq)) {
      if (vb) message("rq is NULL; generating default")
      rq <- make_default_request()
    }
    
    session_metadata <- extract_session_metadata(volume_json)
    if (!(session_id %in% session_metadata$id)) {
      if (vb) message("Session ", session_id, " not found.")
      return(NULL)
    } else {
      rq <- rq |>
        httr2::req_url(sprintf(QUERY_SLOT, session_id))
      resp <- tryCatch(
        httr2::req_perform(rq),
        httr2_error = function(cnd)
          NULL
      )
      if (!is.null(resp)) {
        httr2::resp_body_json(resp)
      } else {
        resp
      }
    }
  }
