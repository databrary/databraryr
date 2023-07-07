#' Download a zip archive with all data from a specified volume and session
#'
#' @param vol_id Volume number.
#' @param session_id Slot/session number.
#' @param out_dir Directory to save output file.
#' @param file_name Name for downloaded file, default is 'test.zip'.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_session_zip()
#' @export
download_session_zip <- function(vol_id = 31,
                                 session_id = 9803,
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
  if (length(session_id) > 1) {
    stop("Session ID must have length 1.")
  }
  if ((!is.numeric(session_id)) || session_id <= 0 ) {
    stop("Session ID must be a number > 0.")
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
  
  url_download <- paste0("https://nyu.databrary.org", paste("/volume", vol_id,
                                                            "slot", session_id,
                                                            "zip/false",
                                                            sep="/"))
  
  if (vb) message(paste0('Sending GET to ', url_download))
  message(paste0("Attempting to download zip archive from volume ", vol_id, ", session ", session_id, "."))
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
        file_name <- make_zip_fn(out_dir, vol_id, session_id) 
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

#-------------------------------------------------------------------------------
make_zip_fn <- function(out_dir, vol_id, session_id) {
  if (!is.character(out_dir)) stop("`out_dir` must be a string")
  if (!is.numeric(vol_id)) stop("`vol_id` must be an integer.")
  if (!is.numeric(session_id)) stop("`session_id` must be an integer")
  paste0(out_dir, "/vol_", vol_id, "-sess_",
         session_id, "-",
         format(Sys.time(), "%F-%H%M-%S"), ".zip")
}
