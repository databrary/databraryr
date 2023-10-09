#' Download Asset From Databrary.
#'
#'#' @description
#' `r lifecycle::badge("superseded")`
#' `download_asset()` has been superseded in favor of `download_video()`.
#' @param asset_id Asset id for target file.
#' @param session_id Slot/session number where target file is stored.
#' @param file_name Name for downloaded file.
#' @param target_dir Directory to save the downloaded file. 
#' Default is a temporary directory given by a call to `tempdir()`.
#' @param return_response A Boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns Full file name to the asset.
#' @examples
#' \dontrun{
#' download_asset() # Download's 'numbers' file from volume 1.
#' download_asset(asset_id = 11643, session_id = 9825, file_name = "rdk.mp4") 
#' # Downloads a display with a random dot kinematogram (RDK).
#' }
#' @export
download_asset <- function(asset_id = 1,
                           session_id = 9807,
                           file_name = "test.mp4",
                           target_dir = tempdir(),
                           return_response = FALSE,
                           vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(asset_id) == 1)
  assertthat::assert_that(is.numeric(asset_id))
  assertthat::assert_that(asset_id >= 1)
  
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  
  assertthat::assert_that(length(file_name) == 1)
  assertthat::assert_that(is.character(file_name))

  assertthat::assert_that(length(target_dir) == 1)
  assertthat::assert_that(is.character(target_dir))
  assertthat::assert_that(dir.exists(target_dir))
  
  assertthat::assert_that(length(return_response) == 1)
  assertthat::assert_that(is.logical(return_response))

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  asset_url <- paste("/slot", session_id, "-", "asset", asset_id,
                     "download", sep = "/")
  url_download <- paste0("https:/nyu.databrary.org", asset_url)
  
  webpage <- rvest::session(url_download)
  if (webpage$response$status_code == 200) {
    content_type <- webpage$response$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query\n")
      message(paste0("Content-type is ", content_type))
    }
    # TODO(somebody): Add support for other content types
    if (content_type == "video/mp4") {
      if (file_name == "test.mp4") {
        if (vb) {
          message("File name unspecified. Generating unique name.\n")
        }
        file_name <- paste0(
          session_id,
          "-",
          asset_id,
          "-",
          format(Sys.time(), "%F-%H%M-%S"),
          ".mp4"
        )
      }
      file_name <- file.path(target_dir, file_name)
      if (vb) {
        message(paste0("Downloading video as ", file_name), "\n")
      }
      utils::download.file(webpage$handle$url,
                           file_name,
                           mode = "wb")
      return(file_name)
    }
    if (content_type == "application/vnd.datavyu") {
      if (vb)
        message("Target file is Datavyu (.opf) spreadsheet.")
      if (file_name == "test.opf") {
        if (vb) {
          message("File name unspecified. Generating unique name.\n")
        }
        file_name <- paste0(
          session_id,
          "-",
          asset_id,
          "-",
          format(Sys.time(), "%F-%H%M-%S"),
          ".opf"
        )
      }
      file_name <- file.path(target_dir, file_name)
      if (vb) {
        message(paste0("Downloading file as '", file_name, "'\n"))
      }
      utils::download.file(webpage$handle$url, file_name, mode = "w")
      return(file_name)
    }
  } else {
    if (vb)
      message(
        paste(
          'Download Failed, HTTP status ',
          webpage$response$status_code,
          '\n',
          sep = ""
        )
      )
    if (return_response)
      return(webpage$response)
    return(NULL)
  }
}
