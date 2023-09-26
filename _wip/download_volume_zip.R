#========================================================================================
#' Download Zip Archive of All Data in a Volume.
#'
#' @param vol_id Volume number.
#' @param out_dir Directory to save output file.
#' @param file_name Name for downloaded file, default is 'test.mp4'.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns Full filename of the downloaded file.
#' @examples
#' download_volume_zip()
#' @export
download_volume_zip <- function(vol_id = 31,
                                out_dir = tempdir(),
                                file_name = "test.zip",
                                vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(out_dir) == 1)
  assertthat::assert_that(is.character(out_dir))
  
  assertthat::assert_that(length(file_name) == 1)
  assertthat::assert_that(is.character(file_name))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # TODO (someone): Convert below to use GET_db_contents()
  url_download <- paste0("https://nyu.databrary.org", paste("/volume", vol_id,
                                                            "zip/false",
                                                            sep="/"))
  
  if (vb) message(paste0('Sending GET to ', url_download))
  message(paste0("Attempting to download zip archive from volume ", vol_id, "."))
  webpage <- httr::GET(url_download, httr::progress())
  if (webpage$status_code == 200) {
    content.type <- webpage$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query.")
      message(paste0("Content-type is ", content.type))
    }
    if (content.type == "application/zip") {
      if (file_name == "test.zip") {
        if (vb) {
          if (vb) message("File name unspecified. Generating unique name.")
        }
        file_name <- paste0(out_dir, "/vol_", vol_id, "-",
                            format(Sys.time(), "%F-%H%M-%S"), ".zip") 
      }
      if (vb) {
        if (vb) message(paste0("Downloading zip file as: \n", file_name))
      }
      bin <- httr::content(webpage, 'raw')
      writeBin(bin, file_name)
      #message(paste0('Downloaded ', file_name))
      file_name
    }
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', webpage$status_code))
  }
}
