#' Download Session Spreadsheet
#'
#' @param vol_id Target volume number.
#' @param file_name Name for the output file. Default is 'test.csv'.
#' @param target_dir Directory to save downloaded file. Default is tempdir().
#' @param as_df A Boolean value. Default is FALSE.
#' @param vb A Boolean value. Default is FALSE.
#' @returns Name of downloaded file or a data frame if `as_df` is TRUE.
#' @examples
#' download_session_csv() # Downloads "session" CSV for volume 1
#' str(download_session_csv(as_df = TRUE))
#' @export
download_session_csv <- function(vol_id = 1,
                                 file_name = "test.csv",
                                 target_dir = tempdir(),
                                 as_df = FALSE,
                                 vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(file_name) == 1)
  assertthat::assert_that(is.character(file_name))
  
  assertthat::assert_that(length(target_dir) == 1)
  assertthat::assert_that(is.character(target_dir))
  
  assertthat::assert_that(length(as_df) == 1)
  assertthat::assert_that(is.logical(as_df))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  full_fn <- file.path(target_dir, file_name)
  
  if (vb)
    message(paste0("Downloading spreadsheet from volume ", vol_id, '.'))
  csv_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  r <-
    GET_db_contents(URL_components = paste0("/volume/", vol_id, "/csv"),
                    vb = vb)
  if (!is.null(r)) {
    if (vb) message("Valid CSV downloaded.")
    if (as_df == TRUE) {
      as.data.frame(r)
    } else {
      if (vb)
        message("Saving CSV.")
      readr::write_csv(as.data.frame(r), full_fn)
      full_fn
    }
  } else {
    if (vb)
      message("No CSV data returned for vol_id ", vol_id)
    r
  }
}
