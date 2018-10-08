#' Get the time range from a given video or audio asset.
#'
#' @param vol.id Volume ID
#' @param session.id Slot/session number.
#' @param asset.id Asset number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' get_asset_segment_range()
#' @export
get_asset_segment_range <- function(vol.id = 1,
                                    session.id = 9807, asset.id = 1,
                                    vb = FALSE) {
  # Test parameters--------------------------------------------------------
  if (!is.numeric(vol.id)) {
    stop("vol.id must be numeric")
  }
  if (vol.id < 1) {
    stop("vol.id must be >= 1")
  }
  if (!is.numeric(session.id)) {
    stop("session.id must be numeric")
  }
  if (session.id < 1) {
    stop("slot.id must be >= 1")
  }
  if (!is.numeric(asset.id)) {
    stop("asset.id must be numeric")
  }
  if (asset.id < 1) {
    stop("asset.id must be >= 1")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical")
  }

  # Retrieve data from Databrary-------------------------------------------
  w <- httr::GET(paste0("https://nyu.databrary.org/api/volume/", vol.id,
                        "/slot/",
                        session.id, "/asset/", asset.id))
  if (httr::status_code(w) == 200) {
    content.type <- w$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query.")
      message(paste0("Content-type is ", content.type))
    }
    r <- jsonlite::fromJSON(httr::content(w, type = 'text', encoding = 'utf8'))
    return(r$segment)
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', w$status_code))
    return(NULL)
  }
}
