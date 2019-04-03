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
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  r <- GET_db_contents(base_URL = "https://nyu.databrary.org/volume/",
                       URL_components = paste0(vol_id, '/csv'),
                       vb = vb, convert_JSON = FALSE)
  if (to_df == TRUE) {
    r_df <- read.csv(text = r)
    if (class(r_df)=="data.frame") {
      r_df <- dplyr::rename(r_df, session_id = session.id,
                            session_name = session.name,
                            session_date = session.date,
                            session_release = session.release)
      return(r_df)
    } else {
      if (vb) message("Can't coerce to data frame. Skipping.\n")
      return(NULL)
    }
  } else {
    return(r)
  }

  # request_url <- paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  # r = httr::GET(paste0(request_url))
  # if (vb) {
  #   message(paste0("Sending GET to ", request_url))
  # }
  # if (httr::status_code(r) == 200){
  #   r_content <- httr::content(r, 'text', encoding = "UTF-8")
  #   if(to_df == TRUE){
  #     r_df <- read.csv(text = r_content)
  #     if (class(r_df)=="data.frame") {
  #       r_df <- dplyr::rename(r_df, session_id = session.id,
  #                             session_name = session.name,
  #                             session_date = session.date,
  #                             session_release = session.release)
  #       return(r_df)
  #     } else {
  #       if (vb) message("Can't coerce to data frame. Skipping.\n")
  #       return(NULL)
  #     }
  #   } else {
  #     return(r_content)
  #   }
  # } else {
  #   message(paste0('Download Failed, HTTP status ', httr::status_code(r)))
  #   if (return_response) return(r)
  # }
}
