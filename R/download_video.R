#' Download Video From Databrary.
#'
#' @param asset_id Asset id for target file.
#' @param session_id Slot/session number where target file is stored.
#' @param file_name Name for downloaded file.
#' @param target_dir Directory to save the downloaded file.
#' Default is a temporary directory given by a call to `tempdir()`.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns Full file name to the asset.
#' @examples
#' download_asset() # Download's 'numbers' file from volume 1.
#' download_asset(asset_id = 11643, session_id = 9825, file_name = "rdk.mp4")
#' # Downloads a display with a random dot kinematogram (RDK).
#' @export
download_video <- function(asset_id = 1,
                           session_id = 9807,
                           file_name = tempfile(paste0(session_id, "_", asset_id),
                                                fileext = ".mp4"),
                           target_dir = tempdir(),
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
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  if (file.exists(file_name)) {
    if (vb)
      message("File exists. Generating new unique name.\n")
    file_name <- file.path(tempdir(),
                           paste0(
                             session_id,
                             "-",
                             asset_id,
                             "-",
                             format(Sys.time(), "%F-%H%M-%S"),
                             ".mp4"
                           ))
  }
  
  if (vb)
    message("Downloading video with asset_id ",
            asset_id,
            " from session_id ",
            session_id)
  
  asset_url <- paste("/slot", session_id, "-", "asset", asset_id,
                     "download", sep = "/")
  url_download <- paste0("https:/nyu.databrary.org", asset_url)
  r <-
    httr::GET(url_download, httr::write_disk(file_name, overwrite = TRUE))
  
  if (httr::status_code(r) == 200) {
    if (vb)
      message("Successful HTML GET query.\n")
    if (r$headers$`content-type` != "video/mp4") {
      message("Content not `video/mp4`.")
      return(NULL)
    } else {
      file_name
    }
  } else {
    if (vb)
      message('Download Failed, HTTP status ', httr::status_code(r))
    return(NULL)
  }
}
