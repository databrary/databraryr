#' Search For Keywords in Databrary Volumes.
#'
#' @param search_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A list with the volumes that contain the keyword.
#' @examples
#' search_for_keywords()
#' @export
search_for_keywords <-
  function(search_string = "locomotion", vb = FALSE) {

    # Check parameters
    assertthat::assert_that(length(search_string) == 1)
    assertthat::assert_that(is.character(search_string))
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    if (vb)
      message('search_for_keywords()...')
    
    # Make URL, GET(), and handle response ---------------------------
    if (vb)
      message(paste0("Searching for ", search_string))
    
    r <- GET_db_contents(
      URL_components = paste0('/api/search?q=',
                              search_string),
      convert_JSON = TRUE,
      vb = vb
    )
    r
  }
