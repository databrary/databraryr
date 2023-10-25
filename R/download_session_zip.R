#' Download Zip Archive From Databrary Session.
#'
#' @param vol_id Volume number.
#' @param session_id Slot/session number.
#' @param out_dir Directory to save output file.
#' @param file_name Name for downloaded file, default is 'test.zip'.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns Full filename of the downloaded file.
#' @examples
#' \donttest{
#' \dontrun{
#' download_session_zip() # Downloads Zip Archive from volume 31, session 9803
#' }
#' }
#' @export
download_session_zip <- function(vol_id = 31,
                                 session_id = 9803,
                                 out_dir = tempdir(),
                                 file_name = "test.zip",
                                 vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  
  assertthat::assert_that(length(out_dir) == 1)
  assertthat::assert_that(is.character(out_dir))
  
  assertthat::assert_that(length(file_name) == 1)
  assertthat::assert_that(is.character(file_name))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  url_download <-
    paste0(
      "https://nyu.databrary.org",
      paste("/volume", vol_id,
            "slot", session_id,
            "zip/false",
            sep = "/")
    )
  
  if (vb)
    message(paste0('Sending GET to ', url_download))
  if (vb)
    message(
      paste0(
        "Attempting to download zip archive from volume ",
        vol_id,
        ", session ",
        session_id,
        "."
      )
    )
  
  bin <- databraryr::GET_db_contents(
    URL_components = paste("/volume", vol_id,
                           "slot", session_id,
                           "zip/false", sep = "/"),
    vb = vb
  )
  if (is.null(bin)) {
    if (vb) message("Null file returned")
    return(NULL)
  }
  
  if (file_name == "test.zip") {
    if (vb) {
      if (vb)
        message("File name unspecified. Generating unique name.")
    }
    file_name <- make_zip_fn_sess(out_dir, vol_id, session_id)
  }
  if (vb) {
    if (vb)
      message(paste0("Downloading zip file as: \n'", file_name, "'."))
  }
  writeBin(bin, file_name)
  file_name
}

#-------------------------------------------------------------------------------
make_zip_fn_sess <- function(out_dir, vol_id, session_id) {
  
  # Check parameters
  assertthat::is.string(out_dir)
  assertthat::is.writeable(out_dir)
  assertthat::assert_that(length(out_dir) == 1)
  
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  
  paste0(
    out_dir,
    "/vol-",
    vol_id,
    "-sess_",
    session_id,
    "-",
    format(Sys.time(), "%F-%H%M-%S"),
    ".zip"
  )
}
