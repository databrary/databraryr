#' @eval options::as_params()
#' @name options_params
#' 
NULL

#' Search For Keywords in Databrary Volumes.
#'
#' @param search_string String to search.
#' @param rq An `httr2` request object. Default is NULL.
#'
#' @returns A list with the volumes that contain the keyword.
#' 
#' @inheritParams options_params
#'
#' @examples
#' \dontrun{
#' search_for_keywords() # searches for volumes with "locomotion" as a keyword.
#' search_for_keywords()
#' 
#' # searches for volumes with "adult" as a keyword.
#' search_for_keywords("adult") 
#' }
#' @export
search_for_keywords <-
  function(search_string = "locomotion",
           vb = options::opt("vb"),
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
        message("Not logged in. Only public information will be returned.")  
      }
      rq <- databraryr::make_default_request()
    }
    rq <- rq %>%
      httr2::req_url(sprintf(QUERY_KEYWORDS, search_string))
    
    if (vb) message("Retrieving data for search string '", search_string, "'.")
    resp <- tryCatch(
      httr2::req_perform(rq),
      httr2_error = function(cnd) {
        NULL
      }
    )
    
    if (vb)
      message('search_for_keywords()...')
    
    if (vb)
      message(paste0("Searching for ", search_string))
    
    if (is.null(resp)) {
      message("Cannot access requested resource on Databrary. Exiting.")
      return(resp)
    } else {
      httr2::resp_body_json(resp)
    }
    #TODO: Reformat search data
  }
