#' Downloads session spreadsheet as a CSV.
#'
#' @param vol_id Target volume number.
#' @param to_df A boolean value.
#' @param return_response A boolean value.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' download_csv()
download_session_csv <- function(vol_id = 1, to_df = TRUE,
                         return_response = FALSE, vb = FALSE) {

  # Error handling
  if (length(vol_id) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  request.url <- paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  r = httr::GET(paste0(request.url))
  if (vb) {
    message(paste0("Sending GET to ", request.url))
  }
  if (httr::status_code(r) == 200){
    r.content <- httr::content(r, 'text', encoding = "UTF-8")
    if(to_df == TRUE){
      r.df <- read.csv(text = r.content)
      if (class(r.df)=="data.frame") {
        r.df <- dplyr::rename(r.df, session_id = session.id,
                              session_name = session.name,
                              session_date = session.date,
                              session_release = session.release)
        return(r.df)
      } else {
        if (vb) message("Can't coerce to data frame. Skipping.\n")
        return(NULL)
      }
    } else {
      return(r.content)
    }
  } else {
    message(paste0('Download Failed, HTTP status ', httr::status_code(r)))
    if (return_response) return(r)
  }
}
