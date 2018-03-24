#' Downloads session spreadsheet as a CSV.
#'
#' @param volume Target volume number.
#' @param to.df A boolean value.
#' @param return.response A boolean value.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' download_csv()
download_csv <- function(volume = 1, to.df = TRUE,
                         return.response = FALSE, vb = FALSE) {
  # Downloads a Databrary session spreadsheet as a CSV.
  #
  # Args:
  #  this.volume: Databrary volume number (integer). Default is 1.
  #  to.df: Flag specifying whether to convert CSV to a data.frame. Default is TRUE.
  #  return.response: Flag specifying whether to return the HTTP response. Default is FALSE.
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.
  #
  # Returns:
  #  A CSV or data frame if available or the HTTP response if requested.

  # Error handling
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || (volume <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  if ((!exists("databrary_config_status")) || (!databrary_config_status)) {
    config_db(vb=vb)
  }
  authenticate_db(vb=vb)

  # GET
  csv.url <- paste0("/volume/", volume, "/csv")
  request.url <- paste0(databrary.url, csv.url)
  r = httr::GET(paste0(request.url))
  if (vb) {
    cat(paste0("sending GET to ", request.url, "\n"))
  }
  if (httr::status_code(r) == 200){
    r.content <- httr::content(r, 'text', encoding = "UTF-8")
    if(to.df == TRUE){
      r.df <- read.csv(text = r.content)
      if (class(r.df)=="data.frame") {
        return(r.df)
      } else {
        if (vb) cat("Can't coerce to data frame. Skipping.\n")
        return(NULL)
      }
    } else {
      return(r.content)
    }
  } else {
    if (vb) cat(paste('Download Failed, HTTP status ', httr::status_code(r), '\n', sep="" ))
    if (return.response) return(r)
  }
}
