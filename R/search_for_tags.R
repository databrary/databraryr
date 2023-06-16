#' Search for tags on volumes or sessions.
#'
#' @param search_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return An array of tags that match the tag_string.
#' @examples
#' search_for_tags()
#' @export
search_for_tags <- function(search_string="ICIS", vb = FALSE) {
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
  if (vb) message('search_for_tags()...')

  # Normalize keyword_string...

  # Make URL, GET(), and handle response ---------------------------
  r <- GET_db_contents(URL_components = paste0('/api/tags/', search_string),
                       vb = vb)
  r
}
