#' Get Data From A Databrary Volume
#'
#' @param vol_id Volume ID.
#' @param vb A logical value. Show verbose messages.
#' @param rq An `httr2` request object.
#' 
#' @returns A JSON blob with the volume data. If the user has previously logged
#' in to Databrary via `login_db()`, then volume(s) that have restricted access
#' can be downloaded, subject to the sharing release levels on those volume(s).
#'
#' @examples
#' \donttest{
#' get_volume_by_id() # Default is Volume 1
#' }
#'
#' @export
get_volume_by_id <- function(vol_id = 1,
                             vb = FALSE,
                             rq = NULL) {
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  # Handle NULL rq
  if (is.null(rq)) {
    if (vb)
      message("rq is NULL; generating default.")
      message("Only public data will be shown.")
    rq <- make_default_request()
  }
  rq <- rq |>
    httr2::req_url(sprintf(GET_VOL_BY_ID, vol_id))
  
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