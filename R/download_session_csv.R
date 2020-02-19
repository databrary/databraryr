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
download_session_csv <- function(vol_id = 1, to_df = TRUE,
                         return_response = FALSE, vb = FALSE) {

  # Error handling
  if (length(vol_id) > 1) {
    stop("vol_id must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("vol_id must be an integer > 0.")
  }

  if (vb) message(paste0("Downloading spreadsheet from volume ", vol_id))
  r <- httr::content(httr::GET(paste0("https://nyu.databrary.org/volume/",
                                      vol_id, "/csv")), 'text', encoding='UTF-8')

  if (is.null(r) | !stringr::str_detect(r, "session-id")) {
      if (vb) message(paste0("No CSV data returned from volume ", vol_id))
      NULL
  } else if (to_df == TRUE) {
      if (vb) message(paste0("Converting response to data frame."))
      r_df <- read.csv(text = r, stringsAsFactors = FALSE)
      if (class(r_df)=="data.frame") {
        if (vb) message(paste0("Imported data frame. Cleaning up."))
        r_df <- dplyr::mutate(r_df, vol_id = vol_id)
        r_df <- dplyr::rename(r_df,
                              session_id = session.id,
                              session_name = session.name,
                              session_date = session.date,
                              session_release = session.release)
        r_df
      } else {
        if (vb) message("Can't coerce to data frame. Skipping.\n")
        NULL
      }
  } else {
    if (vb) message(paste0("Returning raw data from volume ", vol_id))
    r
  }
}
