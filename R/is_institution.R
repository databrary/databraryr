is_institution <- function(party=8, verbose = FALSE) {
  # Tests whether a given party is a Databrary institution or not
  #
  # Args:
  #  party: Number of party. Default us 8 (NYU).
  #  verbose: Flag indicating whether to provide verbose status messages. Default is FALSE
  #
  # Returns:
  #  TRUE or FALSE
  
  # Error handling
  if (length(party) > 1) {
    stop("Party must be single value")
  }
  if ((!is.numeric(party)) || (party <= 0)) {
    stop("Party must be an integer > 0")
  }
  
  party.url <- paste0("/api/party/", party)
  r = GET(paste0(databrary.url, party.url))
  if (status_code(r) == 200) {
    p <- fromJSON(content( r, 'text', encoding = 'UTF-8'))
    if (("institution" %in% names(p)) && (!is.null(p[['institution']]))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if (verbose) {
      cat(paste('Download Failed, HTTP status ', status_code(r), '\n', sep=""))
    }
    return(FALSE)
  }
}

is_person <- function(party = 7){
  return(!is_institution(party))
}