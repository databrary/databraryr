#' Create New Session on A Databrary Volume
#'
#' @param vol_id Volume ID.
#' @param rq An httr2 request object.
#' @param lrsp An httr2 response object returned from `login_db()`.
#' 
#' @returns A JSON blob with the new session's data if the user has previously logged
#' in to Databrary via `login_db()`.
#' 
#' @examples
#' \donttest{
#' \dontrun{
#' # Create `httr2` request objects and log in to Databrary using stored credentials.
#' rq <- make_default_request()
#' lrsp <- make_login_client(email = "YOUR_DATABRARY_EMAIL@INSTITUTION.EDU", store = TRUE, rq = rq)
#' 
#' create_session(vol_id = TARGET_VOLUME_ID, rq, lrsp)
#' }
#' }
#' @export
create_session <- function(vol_id, rq = NULL, lrsp = NULL) {
  
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::is.number(vol_id)
  assertthat::assert_that(!is.null(rq))
  assertthat::assert_that(!is.null(lrsp))
  
  srq <- rq |>
    httr2::req_url(sprintf(CREATE_SLOT, vol_id)) |>
    httr2::req_body_form(csverf = httr2::resp_body_json(lrsp)$csverf)
  
  resp <- tryCatch(
    httr2::req_perform(srq),
    httr2_error = function(cnd)
      NULL
  )
  if (!is.null(resp)) {
    new_session_id <- httr2::resp_body_json(resp)$id
    assertthat::is.string(new_session_id)
    this_volume <- get_volume_by_id(vol_id, rq)
    if (!is.null())
    get_session_by_id(this_volume, as.numeric(new_session_id), rq)
  } else {
    message("No session created")
    NULL
  }
}

refresh_volume <- function(vol_id, rq) {
  get_volume_by_id(vol_id, rq)
}
