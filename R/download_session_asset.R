#' Download Video From Databrary.
#'
#' @param asset_id Asset id for target file.
#' @param session_id Slot/session number where target file is stored.
#' @param file_name Name for downloaded file. Default is NULL.
#' @param target_dir Directory to save the downloaded file.
#' Default is a temporary directory given by a call to `tempdir()`.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object.
#' @returns Full file name to the asset.
#' @examples
#' \donttest{
#' download_asset() # Download's 'numbers' file from volume 1.
#' download_asset(asset_id = 11643, session_id = 9825, file_name = "rdk.mp4")
#' #' # Downloads a display with a random dot kinematogram (RDK).
#' }
#' @export
download_session_asset <- function(asset_id = 1,
                                   session_id = 9807,
                                   file_name = NULL,
                                   target_dir = tempdir(),
                                   vb = FALSE,
                                   rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(asset_id) == 1)
  assertthat::assert_that(is.numeric(asset_id))
  assertthat::assert_that(asset_id >= 1)
  
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  
  assertthat::assert_that(length(target_dir) == 1)
  assertthat::assert_that(is.character(target_dir))
  assertthat::assert_that(dir.exists(target_dir))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  if (is.null(rq))
    rq <- make_default_request()
  
  this_rq <- rq |>
    httr2::req_url(sprintf(DOWNLOAD_FILE, session_id, asset_id)) |>
    httr2::req_progress()
  
  if (vb)
    message("Downloading video with asset_id ",
            asset_id,
            " from session_id ",
            session_id)
  
  resp <- tryCatch(
    httr2::req_perform(this_rq),
    httr2_error = function(cnd)
      NULL
  )
  
  # Gather asset format info
  this_file_extension <- list_asset_formats(vb = vb) |>
    dplyr::filter(httr2::resp_content_type(resp) == format_mimetype) |>
    dplyr::select(format_extension) |>
    as.character()
  
  # Check file name or generate
  if (is.null(this_file_extension)) {
    if (vb)
      message("No matching file extension for ",
              httr2::resp_content_type(resp))
    return(NULL)
  }
  
  if (is.null(file_name)) {
    if (vb)
      message("Missing file name, creating temporary file name.")
    file_name = tempfile(paste0(session_id, "_", asset_id, "_"),
                         fileext = paste0(".", this_file_extension))
  }
  
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
                             paste0(".", this_file_extension)
                           ))
  }
  
  if (!(this_file_extension == xfun::file_ext(file_name))) {
    if (vb)
      message("File name ",
              file_name,
              " doesn't match extension ",
              this_file_extension)
    return(NULL)
  }
  
  write_file <- tryCatch(
    error = function(cnd) {
      if (vb) message("Failure writing file ", file_name)
      NULL
    },
    {
      file_con <- file(file_name, "wb")
      writeBin(resp$body, file_con)
      close(file_con)      
    }
  )
  
  if(!is.null(write_file)) file_name
}