#' Get Data From A Databrary Volume
#'
#' @param vol_id Volume ID.
#' @param rq An httr2 request object.
#' @returns A JSON blob with the volume data. If the user has previously logged
#' in to Databrary via `login_db()`, then volume(s) that have restricted access
#' can be downloaded, subject to the sharing release levels on those volume(s).
#' @examples
#' \donttest{
#' get_volume_by_id() # Default is Volume 1
#' }
#' @export
get_volume_by_id <- function(vol_id = 1, rq = NULL) {
  assertthat::is.number(vol_id)
  
  if (is.null(rq)) {
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