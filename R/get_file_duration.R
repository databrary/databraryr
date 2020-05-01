#' Get the the duration (in ms) of a file.
#'
#' @param asset_id Asset number.
#' @param types_w_durations Asset types that have valid durations.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' get_file_duration() # default is the test video from databrary.org/volume/1
#' @export
get_file_duration <- function(asset_id = 1,
                              types_w_durations = c("-600", "-800"),
                              vb = FALSE) {
# Test parameters---------------------------------------------------------
  if (length(asset_id) > 1) {
    stop("asset_id must have length == 1.")
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

  r <- GET_db_contents(URL_components = paste0('/api/asset/', asset_id),
                       vb = vb)
  if (r$format %in% types_w_durations) {
    r$duration
  } else {
    if (vb) message("File type does not have a defined duration.")
    return(NULL)
  }
}
