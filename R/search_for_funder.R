#' Report Information About A Funder.
#'
#' @param search_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about the funder.
#' @examples
#' \donttest{
#' search_for_funder("national+science+foundation")
#' }
#' @export
search_for_funder <-
  function(search_string = "national+science+foundation",
           vb = FALSE) {
    
    # Check parameters
    assertthat::assert_that(length(search_string) == 1)
    assertthat::assert_that(is.character(search_string))
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    if (vb)
      message('search_for_keywords()...')

    # Make URL, GET(), and handle response ---------------------------
    r <-
      GET_db_contents(URL_components = paste0('/api/funder?query=', search_string),
                      vb = vb)
    if (is.null(r))
      if (vb) message("May need to log in to Databrary via `login_db()`")
    r
  }
