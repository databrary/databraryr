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
                                vb=FALSE) {
  # Parameter checking ----------------------------------------------------------------
  if (length(vol_id) > 1) {
    stop("`vol_id` must have length 1.")
  }
  if ((!is.numeric(vol_id)) || vol_id <= 0 ) {
    stop("`vol_id` must be a number > 0.")
  }
  if (length(out_dir) > 1) {
    stop("`out_dir` must have length 1.")
  }
  if (!(is.character(out_dir))) {
    stop("`out_dir` must be a character string.")
  }
  if (length(file_name) > 1) {
    stop("File name must have length 1.")
  }
  if (!is.character(file_name)) {
    stop("File name must be character string.")
  }
  if (!is.logical(vb)) {
    stop("`vb` must be logical value.")
  }
  if (length(vb) > 1) {
    stop("`vb` must have length == 1.")
  }
  
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
