#' Download Session Spreadsheet As CSV
#'
#' @description Databrary generates a CSV-formated spreadsheet that summarizes
#' information about individual sessions. This command downloads that CSV file
#' as a temporary file or with a name specified by the user.
#'
#' @param vol_id Target volume number.
#' @param file_name Name for the output file. Default is 'test.csv'.
#' @param target_dir Directory to save downloaded file. Default is tempdir().
#' @param as_df A Boolean value. Default is FALSE.
#' @param vb A Boolean value. Default is FALSE.
#' @param req An `httr2` request object.
#' @returns A character string that is the name of the downloaded file or a data frame if `as_df` is TRUE.
#' @examples
#' \donttest{
#' \dontrun{
#' download_session_csv() # Downloads "session" CSV for volume 1
#' }
#' }
#' @export
download_session_csv <- function(vol_id = 1,
                                 file_name = "test.csv",
                                 target_dir = tempdir(),
                                 as_df = FALSE,
                                 vb = FALSE,
                                 rq = NULL) {
  
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
  
  if (is.null(rq))
    rq <- make_default_request()
  this_rq <- rq |>
    httr2::req_url(sprintf(GET_SESSION_CSV, vol_id))
  
  resp <- tryCatch(
    httr2::req_perform(this_rq),
    httr2_error = function(cnd)
      NULL
  )
  
  # r <-
  #   GET_db_contents(URL_components = paste0("/volume/", vol_id, "/csv"),
  #                   vb = vb)
  if (!is.null(resp)) {
    if (vb)
      message("Valid CSV downloaded.")
    resp_txt <- httr2::resp_body_string(resp)
    df <- readr::read_csv(resp_txt, show_col_types = FALSE)
    if (as_df == TRUE) {
      df
    } else {
      if (vb)
        message("Saving CSV.")
      readr::write_csv(df, full_fn)
      #readr::write_csv(as.data.frame(r), full_fn)
      full_fn
    }
  } else {
    if (vb)
      message("No CSV data returned for vol_id ", vol_id)
    resp
  }
}
