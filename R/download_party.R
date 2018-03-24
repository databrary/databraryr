#' Lists basic information about people on Databrary.
#'
#' @param party Party number to download.
#' @param to.df A Boolean value if TRUE returns a data frame.
#' @param convert.JSON A Boolean value if TRUE converts the JSON download
#' @param return.response A Boolean value if TRUE returns the GET response.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return Status code if successful.
#' @examples
#' download_party()
download_party <- function(party = 6, to.df = FALSE,
                           convert.JSON = TRUE,
                           return.response = FALSE, vb = FALSE) {
  # Downloads info from specified Databrary party (individual/institution)
  # and converts to R data frame if desired
  #
  # Args:
  #  party: Databrary party index (integer). Default is 6.
  #  to.df: Flag specifying whether to downloaded data to a data.frame. Default is TRUE.
  #  return.response: Flag specifying whether to return the HTTP response. Default is FALSE.
  #  verbose: Flag indicating whether to provide verbose status messages. Default is FALSE.
  #
  # Returns:
  #  HTTP response as raw (JSON) or CSV if available.

  # Error handling
  if (length(party) > 1) {
    stop("Party must be single value")
  }
  if ((!is.numeric(party)) || (party <= 0)) {
    stop("Party must be an integer > 0")
  }

  if (!exists("databrary_config_status")) {
    config_db(vb = vb)
  }
  authenticate_db()

  party.url <- paste0("/api/party/", party)
  if (vb) {
    cat(paste0("Sending GET to ", party.url, "\n."))
  }
  r = httr::GET(paste0(databrary.url, party.url))

  if (httr::status_code(r) == 200) {
    r.content <- httr::content(r, 'text', encoding = 'UTF-8' )
    if(to.df == TRUE){
      return(read.csv(text = r.content))
    } else if(convert.JSON) {
      return(jsonlite::fromJSON(r.content))
    } else {
      return(r.content)
    }
  } else {
    if (vb) {
      cat(paste('Download Failed, HTTP status ', httr::status_code(r), '\n', sep=""))
    }
    return(r)
  }
}
