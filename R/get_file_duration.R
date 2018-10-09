#' Get the the duration (in ms) of a file.
#'
#' @param asset.id Asset number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' get_file_duration()
#' @export
get_file_duration <- function(asset.id = 1,
                              types.w.durations = c("-600", "-800"),
                              vb = FALSE) {
# Test parameters---------------------------------------------------------
  if (!is.numeric(asset.id)) {
    stop("asset.id must be numeric")
  }
  if (asset.id < 1) {
    stop("asset.id must be >= 1")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical")
  }

# Retrieve file parameters------------------------------------------------
  video_duration_ms = NULL
  g <- httr::GET(paste0("https://nyu.databrary.org/api/asset/", asset.id))
  if (httr::status_code(g) == 200) {
    if (vb) {
      message("Successful HTML GET query.")
      message("Extracting file duration.")
    }
    file_info <- jsonlite::fromJSON(httr::content(g, type = 'text', encoding = 'utf8'))
    if (file_info$format %in% types.w.durations) {
      file_info$duration
    } else {
      if (vb) message("File type does not have a defined duration.")
      return(NULL)
    }
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', httr::status_code(g)))
    return(NULL)
  }
}
