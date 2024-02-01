#' Get Session Info From A Volume As A Data Frame.
#' 
#' @description
#' `r lifecycle::badge("superseded")`
#' 
#' This function has been superceded by `list_volume_sessions()`.
#'
#' @param vol_id Target volume number.
#' @param vb A boolean value.
#' 
#' @returns A data frame with session information from a volume.
#' 
#' @examples
#' \donttest{
#' get_session_as_df() # Return Session Info from Volume 1.
#' #' }
#' 
#' @export
get_session_as_df <- function(vol_id = 1,
                               vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))  # Error handling
  
  # Main routines
  if (vb)
    message(paste0("Retrieving spreadsheet from volume ", vol_id, '.'))
  csv_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  r <- try(httr::GET(csv_url), silent = TRUE)
  
  if (methods::is(r, 'try-error')) {
    if (vb)
      message ("`try-error` probably due to apostrophe bug in API.")
    return(NULL)
  }
  if ((httr::status_code(r) != 200) | (methods::is(r, 'try-error'))) {
    if (vb)
      message("GET returns error")
    NULL
  } else {
    c <-
      httr::content(
        r,
        as = "parsed",
        type = "text/csv",
        encoding = "utf-8",
        show_col_types = FALSE#,
        # col_types = readr::cols(.default = readr::col_character())
      )
    if (is.null(c)) {
      if (vb)
        message(paste0("No CSV data returned from volume ", vol_id))
      NULL
    } else {
      if (vb)
        message(paste0("Converting response to data frame."))
      if (is.data.frame(c)) {
        if (vb)
          message(paste0("Imported data frame. Cleaning up."))
        r_df <- dplyr::mutate(c, vol_id = vol_id)
        names(r_df) <-
          stringr::str_replace(names(r_df),
                               pattern = '[\\-|\\.]',
                               replacement = '_')
        r_df
      } else {
        if (vb)
          message("Can't coerce to data frame. Skipping.\n")
        NULL
      }
    }
  }
}
