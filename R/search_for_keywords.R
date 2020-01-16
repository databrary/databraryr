#' Search for keywords in Databrary volumes.
#'
#' @param keyword_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A list with the activity history on a slot.
#' @examples
#' search_for_keywords()
#' @export
search_for_keywords <- function(search_string="locomotion", vb = TRUE) {
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
  if (vb) message(paste0("Searching for ", search_string))

  r <- GET_db_contents(URL_components = paste0('/api/search?q=',
                                               search_string),
                       convert_JSON = TRUE,
                       vb = vb)
  r
}
