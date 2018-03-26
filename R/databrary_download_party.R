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
  #  vb: Flag indicating whether to provide verbose status messages. Default is FALSE.
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
  source("databrary_authenticate.R")
  authenticate_db()

  party.url <- paste("/api/party/", party, sep="")
  r = httr::GET(paste(databrary.url, party.url, sep=""))

  if (status_code(r) == 200) {
    r.content <- httr::content( r, 'text', encoding = 'UTF-8' )
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
