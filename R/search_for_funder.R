#' Search for funders.
#'
#' @param search_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A list with the activity history on a slot.
#' @examples
#' search_for_funder()
#' @export
search_for_funder <- function(search_string="national+science+foundation", 
                              vb = FALSE) {
  # Parameter checking----------------------------------------------
  if (!is.character(search_string)) {
    stop("search_string must be string.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }
  if (vb) message('search_for_keywords()...')
  
  # Test if logged in
  if (!file.exists('.databrary.RData')){
    if (vb) {
      message("Must log-in to Databrary.")
      databraryr::login_db()
    }
  }
  
  # Make URL, GET(), and handle response ---------------------------
  r <- GET_db_contents(URL_components = paste0('/api/funder?query=', search_string),
                       vb = vb)
  r
}
