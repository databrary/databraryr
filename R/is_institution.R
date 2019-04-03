#' Tests whether given party ID is from an institution.
#'
#' @param party_id Databrary party ID
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return TRUE or FALSEl.
#' @examples
#' is_institution()
#' @export
is_institution <- function(party_id=8, vb = FALSE) {
  # Error handling
  if (length(party_id) > 1) {
    stop("party_id must be single value")
  }
  if ((!is.numeric(party_id)) || (party_id <= 0)) {
    stop("party_id must be an integer > 0")
  }

  # Process request-----------------------------------------------------
  r <- GET_db_contents(URL_components = paste0('party/', party_id),
                       vb = vb)
  if (("institution" %in% names(r)) && (!is.null(r[['institution']]))) {
    return(TRUE)
  } else {
    return(FALSE)
  }

  # party.url <- paste0("https://nyu.databrary.org/api/party/", party_id)
  # if (vb) message(paste0("Sending GET to ", party.url))
  # r = httr::GET(party.url)
  # if (httr::status_code(r) == 200) {
  #   p <- jsonlite::fromJSON(httr::content( r, 'text', encoding = 'UTF-8'))
  #   if (("institution" %in% names(p)) && (!is.null(p[['institution']]))) {
  #     return(TRUE)
  #   } else {
  #     return(FALSE)
  #   }
  # } else {
  #   if (vb) {
  #     cat(paste0('Download Failed, HTTP status ', httr::status_code(r), '\n'))
  #   }
  #   return(FALSE)
  # }
}

#' Tests whether given party ID is from a person.
#'
#' @param party_id Databrary party ID
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return TRUE or FALSEl.
#' @examples
#' is_person()
#' @export
is_person <- function(party_id = 7, vb = FALSE){
  return(!is_institution(party_id, vb = vb))
}
