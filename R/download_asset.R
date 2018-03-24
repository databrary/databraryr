#' Download a specific video file.
#'
#' @param asset Asset number.
#' @param slot Slot/session number.
#' @param file.name Name for downloaded file.
#' @param return.respose A Boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_asset()
download_asset <- function(asset = 11643, slot = 9825,
                           file.name = "test.mp4",
                           return.response=FALSE, vb=FALSE) {
  # Downloads a Databrary asset given a volume, slot, asset and segment.
  #
  # Args:
  #  asset: Databrary asset number (integer). Default is 11643.
  #  slot: Databrary slot number (integer). Default is 9825.
  #  return.response: Flag specifying whether to return the HTTP response. Default is FALSE.
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.
  #
  # Returns:
  #  A given asset saved as a file.

  # Error handling
  if (!is.character(file.name)) {
    stop("File.name must be character string.")
  }
  if (length(slot) > 1) {
    stop("Slot must have length 1.")
  }
  if ((!is.numeric(slot)) || slot <= 0 ) {
    stop("Slot must be > 0.")
  }
  if (length(asset) > 1) {
    stop("Asset must have length 1.")
  }
  if ((!is.numeric(asset)) || asset <= 0 ) {
    stop("Asset must be > 0.")
  }

  if ((!exists("databrary_config_status")) || (!databrary_config_status)) {
    config_db(vb=vb)
  }
  authenticate_db(vb=vb)

  asset.url <- paste("/slot", slot, "-", "asset", asset,
                     "download", sep="/")
  url.download <- paste0(databrary.url, asset.url)

  webpage <- rvest::html_session(url.download)
  if (webpage$response$status_code == 200) {
    content.type <- webpage$response$headers$`content-type`
    if (vb) {
      cat("Successful HTML GET query\n")
      cat(paste0("Content-type is ", content.type, ".\n"))
    }
    # TODO(somebody): Add support for other content types
    if (content.type == "video/mp4") {
      if (file.name == "test.mp4") {
        if (vb) {
          cat("File name unspecified. Generating unique name.\n")
        }
        file.name <- paste0(slot, "-", asset, "-", format(Sys.time(), "%F-%H%M-%S"), ".mp4")
      }
      if (vb) {
        cat(paste0("Downloading video as ", file.name, "\n"))
      }
      download.file(webpage$handle$url, file.name, mode = "wb")
    }
  } else {
    if (vb) cat(paste('Download Failed, HTTP status ', webpage$response$status_code, '\n', sep="" ))
    if (return.response) return(webpage$response)
  }
}
