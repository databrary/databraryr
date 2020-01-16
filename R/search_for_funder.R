#' Search for funders.
#'
#' @param keyword_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A list with the activity history on a slot.
#' @examples
#' search_for_funder()
#' @export
search_for_funder <- function(search_string="national+science+foundation", vb = FALSE) {
  # Parameter checking----------------------------------------------
  if (!is.character(search_string)) {
    stop("search_string must be string.")
  }
  if (length(vb) > 1) {se
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }
  if (vb) message('search_for_keywords()...')

  # Normalize keyword_string...

  # Make URL, GET(), and handle response ---------------------------
  r <- GET_db_contents(URL_components = paste0('/api/funder?query=', search_string),
                       vb = vb)
  r
}
