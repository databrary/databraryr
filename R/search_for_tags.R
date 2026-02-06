#' @eval options::as_params()
#' @name options_params
#' 
NULL

#' Search For Tags on Volumes or Sessions.
#'
#' @param search_string String to search.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is NULL.
#'
#' @returns An array of tags that match the tag_string.
#' 
#' @inheritParams options_params
#'
#' @examples
#' \dontrun{
#' search_for_tags() # Searches for volumes that have the tag "ICIS"
#' }
#'
#' @export
search_for_tags <-
  function(search_string = "ICIS",
           vb = options::opt("vb"),
           rq = NULL) {
    # Check parameters
    assertthat::assert_that(length(search_string) == 1)
    assertthat::assert_that(is.character(search_string))
    
    validate_flag(vb, "vb")
    
    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))
    
  results <- collect_paginated_get(
    path = API_SEARCH_VOLUMES,
    params = list(tag = search_string),
    rq = rq,
    vb = vb
  )

  if (is.null(results) || length(results) == 0) {
    if (vb) message("No volumes tagged '", search_string, "'.")
    return(NULL)
  }
  
  purrr::map_dfr(results, function(entry) {
    tibble::tibble(
      vol_id = entry$id,
      vol_title = entry$title,
      vol_sharing_level = entry$sharing_level,
      vol_tags = list(entry$tags),
      score = entry$score
    )
  })
  }
