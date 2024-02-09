#' Report Information About A Funder.
#'
#' @param search_string String to search.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object. Default is NULL. 
#'
#' @returns A data frame with information about the funder.
#' 
#' @examples
#' \donttest{
#' search_for_funder("national+science+foundation")
#' }
#' 
#' @export
search_for_funder <-
  function(search_string = "national+science+foundation",
           vb = FALSE,
           rq = NULL) {
    
    # Check parameters
    assertthat::assert_that(length(search_string) == 1)
    assertthat::assert_that(is.character(search_string))
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))
    
    if (is.null(rq)) {
      if (vb) {
        message("NULL request object. Will generate default.")
        message("\nNot logged in. Only public information will be returned.")  
      }
      rq <- databraryr::make_default_request()
    }
    rq <- rq %>%
      httr2::req_url(sprintf(QUERY_VOLUME_FUNDER, search_string))
    
    resp <- tryCatch(
      httr2::req_perform(rq),
      httr2_error = function(cnd) {
        NULL
      }
    )
    
    if (vb)
      message('search_for_keywords()...')

    if (!is.null(resp)) {
      httr2::resp_body_json(resp) %>% as.data.frame()
    } else {
      resp
    }
  }
