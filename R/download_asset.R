#' Download a specific video file.
#'
#' @param asset Asset number.
#' @param slot Slot/session number.
#' @param file.name Name for downloaded file.
#' @param return.response A Boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_asset()
download_asset <- function(asset = 11643, slot = 9825,
                           file.name = "test.mp4",
                           return.response=FALSE, vb=FALSE) {

  # Error handling
  if (length(asset) > 1) {
    stop("Asset ID must have length 1.")
  }
  if ((!is.numeric(asset)) || asset <= 0 ) {
    stop("Asset ID must be number > 0.")
  }
  if (length(slot) > 1) {
    stop("Session ID must have length 1.")
  }
  if ((!is.numeric(slot)) || slot <= 0 ) {
    stop("Session ID must be number > 0.")
  }
  if (!is.character(file.name)) {
    stop("File name must be character string.")
  }

  # if ((!exists("databrary_config_status")) || (!databrary_config_status)) {
  #   databraryapi::config_db(vb = vb)
  # }
  #authenticate_db(vb = vb)

  asset.url <- paste("/slot", slot, "-", "asset", asset,
                     "download", sep="/")
  url.download <- paste0("https:/nyu.databrary.org", asset.url)

  webpage <- rvest::html_session(url.download)
  if (webpage$response$status_code == 200) {
    content.type <- webpage$response$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query\n")
      message(paste0("Content-type is ", content.type))
    }
    # TODO(somebody): Add support for other content types
    if (content.type == "video/mp4") {
      if (file.name == "test.mp4") {
        if (vb) {
          message("File name unspecified. Generating unique name.\n")
        }
        file.name <- paste0(slot, "-", asset, "-", format(Sys.time(), "%F-%H%M-%S"), ".mp4")
      }
      if (vb) {
        message(paste0("Downloading video as ", file.name, "\n"))
      }
      utils::download.file(webpage$handle$url, file.name, mode = "wb")
    }
  } else {
    if (vb) message(paste('Download Failed, HTTP status ', webpage$response$status_code, '\n', sep="" ))
    if (return.response) return(webpage$response)
  }
}
