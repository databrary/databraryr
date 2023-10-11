#' Search For Tags on Volumes or Sessions.
#'
#' @param search_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns An array of tags that match the tag_string.
#' @examples
#' \dontrun{
#' search_for_tags() # Searches for volumes that have the tag "ICIS"
#' #' }
#' @export
search_for_tags <- function(search_string="ICIS", vb = FALSE) {

  # Check parameters
  assertthat::assert_that(length(search_string) == 1)
  assertthat::assert_that(is.character(search_string))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Make URL, GET(), and handle response ---------------------------
  r <- GET_db_contents(URL_components = paste0('/api/tags/', search_string),
                       vb = vb)
  
  r
}
