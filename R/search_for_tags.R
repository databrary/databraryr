#' Search For Tags on Volumes or Sessions.
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
  
  # Make URL, GET(), and handle response ---------------------------
  r <- GET_db_contents(URL_components = paste0('/api/tags/', search_string),
                       vb = vb)
  
  # https://nyu.databrary.org/search?offset=0&volume&f.tag_name="infant%20rat"
  # r <- GET_db_contents(URL_components = paste0('/search?offset=0&volume&f.tag_name="', search_string, '"'),
  #                      vb = vb)
  # r <- httr::GET('https://nyu.databrary.org/search?offset=0&volume&f.tag_name="infant+rat"&limit=8')
  
  r
}
