#' Downloads session spreadsheet as a CSV.
#'
#' @param volume Target volume number.
#' @param to.df A boolean value.
#' @param return.response A boolean value.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' download_csv()
download_session_csv <- function(volume = 1, to.df = TRUE,
                         return.response = FALSE, vb = FALSE) {

  # Error handling
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || (volume <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  if ((!exists("databrary_config_status")) || (!databrary_config_status)) {
    databraryapi::config_db(vb=vb)
  }
  databraryapi::authenticate_db(vb=vb)

  # GET
  csv.url <- paste0("/volume/", volume, "/csv")
  request.url <- paste0(databrary.url, csv.url)
  r = httr::GET(paste0(request.url))
  if (vb) {
    message(paste0("sending GET to ", request.url))
  }
  if (httr::status_code(r) == 200){
    r.content <- httr::content(r, 'text', encoding = "UTF-8")
    if(to.df == TRUE){
      r.df <- read.csv(text = r.content)
      if (class(r.df)=="data.frame") {
        return(r.df)
      } else {
        if (vb) message("Can't coerce to data frame. Skipping.\n")
        return(NULL)
      }
    } else {
      return(r.content)
    }
  } else {
    if (vb) message(paste('Download Failed, HTTP status ', httr::status_code(r), '\n', sep="" ))
    if (return.response) return(r)
  }
}
