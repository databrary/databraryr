#' Get the time range from a given video or audio asset.
#'
#' @param vol_id Volume ID
#' @param session_id Slot/session number.
#' @param asset_id Asset number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' get_asset_segment_range()
#' @export
get_asset_segment_range <- function(vol_id = 1,
                                    session_id = 9807, asset_id = 1,
                                    vb = FALSE) {
  # Test parameters--------------------------------------------------------
  if (length(vol_id) > 1) {
    stop("vol_id must have length == 1.")
  }
  if (!is.numeric(vol_id)) {
    stop("vol_id must be numeric")
  }
  if (vol_id < 1) {
    stop("vol_id must be >= 1")
  }
  if (length(session_id) > 1) {
    stop("session_id must have length == 1.")
  }
  if (!is.numeric(session_id)) {
    stop("session_id must be numeric")
  }
  if (length(asset_id) > 1) {
    stop("asset_id must have length == 1.")
  }
  if (session_id < 1) {
    stop("slot.id must be >= 1")
  }
  if (!is.numeric(asset_id)) {
    stop("asset_id must be numeric")
  }
  if (asset_id < 1) {
    stop("asset_id must be >= 1")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical")
  }

  # Retrieve data from Databrary-------------------------------------------
  w <- httr::GET(paste0("https://nyu.databrary.org/api/volume/", vol_id,
                        "/slot/",
                        session_id, "/asset/", asset_id))
  if (httr::status_code(w) == 200) {
    content_type <- w$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query.")
      message(paste0("Content-type is ", content_type))
    }
    r <- jsonlite::fromJSON(httr::content(w, type = 'text', encoding = 'utf8'))
    return(r$segment)
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', w$status_code))
    return(NULL)
  }
}
