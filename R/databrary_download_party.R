databrary_download_party <- function(party = 6, to.df = FALSE,
                                     convert.JSON = TRUE,
                                     return.response = FALSE, verbose = FALSE) {
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
  
  require("jsonlite")
  if (!exists("databrary_config_status")) {
    source("databrary_config.R")
    databrary_config(verbose = verbose)
  }
  source("databrary_authenticate.R")
  databrary_authenticate()
  
  party.url <- paste("/api/party/", party, sep="")
  r = GET(paste(databrary.url, party.url, sep=""))
  
  if (status_code(r) == 200) {
    r.content <- content( r, 'text', encoding = 'UTF-8' )
    if(to.df == TRUE){
      return(read.csv(text = r.content))
    } else if(convert.JSON) {
      return(fromJSON(r.content))
    } else {
      return(r.content)      
    }
  } else {
    if (verbose) {
      cat(paste('Download Failed, HTTP status ', status_code(r), '\n', sep=""))
    }
    return(r)
  }
}