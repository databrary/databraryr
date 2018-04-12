#' Tests whether given party ID is from an institution.
#'
#' @param party Databrary party ID
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return TRUE or FALSEl.
#' @examples
#' is_institution()
is_institution <- function(party=8, vb = FALSE) {
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
  if (vb) message("Sending GET to ", paste0(databrary.url, party.url))
  r = httr::GET(paste0(databrary.url, party.url))
  if (httr::status_code(r) == 200) {
    p <- jsonlite::fromJSON(httr::content( r, 'text', encoding = 'UTF-8'))
    if (("institution" %in% names(p)) && (!is.null(p[['institution']]))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    if (vb) {
      cat(paste0('Download Failed, HTTP status ', httr::status_code(r), '\n'))
    }
    return(FALSE)
  }
}


#' Tests whether given party ID is from a person.
#'
#' @param party Databrary party ID
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return TRUE or FALSEl.
#' @examples
#' is_person()
is_person <- function(party = 7, vb = vb){
  return(!is_institution(party, vb = vb))
}
