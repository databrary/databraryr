#' Get the time range from a given video or audio asset.
#'
#' @param vol_id Volume ID
#' @param session_id Slot/session number.
#' @param asset_id Asset number.
#' @param convert_JSON A Boolean value. If TRUE, convert JSON to a data frame. Default is TRUE.
#' @param segment_only A Boolean value. If TRUE, returns only the segment values. Otherwise returns
#' a data frame with two fields, segment and permission. Default is TRUE.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' get_asset_segment_range()
#' @export
get_asset_segment_range <- function(vol_id = 1,
                                    session_id = 9807, asset_id = 1,
                                    convert_JSON = TRUE,
                                    segment_only = TRUE,
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

  r <- GET_db_contents(URL_components = paste0('/api/volume/', vol_id,"/slot/",
                                               session_id, "/asset/",
                                               asset_id),
                       vb = vb,
                       convert_JSON = convert_JSON)
  if (vb) {
    message("Returning segment start & end times (in ms) from volume ", vol_id, 
            ", session ", session_id, ", asset ", asset_id)
  }
  if (segment_only) {
    return(r$segment)
  } else {
    return(r)
  }
}
