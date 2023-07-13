#' Downloads session spreadsheet as a CSV.
#'
#' @param vol_id Target volume number.
#' @param file_name Name for the output file. Default is 'test.csv'.
#' @param target_dir Directory to save downloaded file. Default is '.'.
#' @param vb A boolean value.
#' @returns Name of downloaded file.
#' @examples
#' download_session_csv() # Downloads "session" CSV for volume 1
#' @export
download_session_csv <- function(vol_id = 1,
                         file_name = "test.csv",
                         target_dir = tempdir(),
                         vb = FALSE) {
  # Error handling
  if (length(vol_id) > 1) {
    stop("`vol_id` must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("`vol_id` must be an integer > 0.")
  }
  if (!is.character(file_name)) {
    stop("`file_name` must be character string.")
  }
  if ((!is.character(target_dir))) {
    stop("`target_dir` must be character string.")
  }
  if (!dir.exists(target_dir)) {
    stop("target_dir '", target_dir, "' not found.")
  }
  if (!is.logical(vb)) {
    stop("`vb` must be a logical value.")
  }
  if (length(vb) > 1) {
    stop("`vb` must have length 1.")
  }
  
  full_fn <- file.path(target_dir, file_name)
  
  if (vb)
    message(paste0("Downloading spreadsheet from volume ", vol_id, '.'))
  csv_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  r <- try(rvest::session(csv_url), silent = TRUE)
  
  if (methods::is(r, 'try-error')) {
    if (vb)
      message ("`try-error` probably due to apostrophe bug in Databrary API.")
    return(NULL)
  }
  if ((r$response$status_code != 200) | (methods::is(r, 'try-error')))
  {
    if (vb)
      message("GET returns error")
    NULL
  } else {
    utils::download.file(r$handle$url, full_fn, mode = "w")
    full_fn
  }
}
