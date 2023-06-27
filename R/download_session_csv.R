#' Downloads session spreadsheet as a CSV.
#'
#' @param vol_id Target volume number.
#' @param to_df A boolean value.
#' @param return_response A boolean value.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' download_csv()
#' @export
download_session_csv <- function(vol_id = 1,
                                 to_df = TRUE,
                                 return_response = FALSE,
                                 vb = FALSE) {
  # Error handling
  if (length(vol_id) > 1) {
    stop("`vol_id` must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("`vol_id` must be an integer > 0.")
  }
  
  if (!is.logical(to_df)) {
    stop("`to_df` must be a logical value.")
  }
  if (length(to_df) > 1) {
    stop("`to_df` must have length 1.")
  }
  
  if (!is.logical(return_response)) {
    stop("`return_response` must be a logical value.")
  }
  if (length(return_response) > 1) {
    stop("`return_response` must have length 1.")
  }
  
  if (!is.logical(vb)) {
    stop("`vb` must be a logical value.")
  }
  if (length(vb) > 1) {
    stop("`vb` must have length 1.")
  }
  
  # Main routines
  if (vb)
    message(paste0("Downloading spreadsheet from volume ", vol_id, '.'))
  csv_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  r <- try(httr::GET(csv_url), silent = TRUE)
  
  if ((class(r) == 'try-error')) {
    if (vb) message ("`try-error` probably due to apostrophe bug in API.")
    return(NULL)
  }
  if ((httr::status_code(r) != 200) | (class(r) == 'try-error')) {
    if (vb) message("GET returns error")
    NULL
  } else {
    if (return_response) {
      r
    } else {
      c <-
        httr::content(
          r,
          show_col_types = FALSE,
          type = 'text/csv',
          encoding = 'utf-8'
          # col_types = readr::cols(.default = 'c')
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
          names(r_df) <- stringr::str_replace(names(r_df), pattern = '[\\-|\\.]', replacement = '_')
          r_df
        } else {
          if (vb)
            message("Can't coerce to data frame. Skipping.\n")
          NULL
        }
      }
    }
  }
}
