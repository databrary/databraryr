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
  r <- GET_db_contents(URL_components = paste0('/api/party/', party_id),
                       vb = vb)
  if (("institution" %in% names(r)) && (!is.null(r[['institution']]))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
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
