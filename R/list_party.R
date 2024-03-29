#' List Data For A Party (person or institution).
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated and may be removed in a future release.
#' See `get_party_by_id()` for similar functionality.
#'
#' @param party_id Target volume number. Default is volume 8 (NYU).
#' @param component Which data to return 'children', 'parents', or 'all'. Default is all.
#' @param vb A Boolean value. If TRUE provides verbose output. Default is FALSE.
#' 
#' @returns A data frame with information about a party (person or institution).
#' 
#' @examples
#' \donttest{
#' list_party() # Default is New York University (party 8)
#' #' }
#' 
#' @export
list_party <- function(party_id = 8,
                       component = 'all',
                       vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id >= 1)
  
  assertthat::assert_that(length(component) == 1)
  assertthat::assert_that(is.character(component))
  assertthat::assert_that(component >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  if (vb)
    message(paste0("Getting sponsors for party ", party_id, "."))
  g <-
    databraryr::GET_db_contents(
      URL_components = paste0("/api/party/", party_id,
                              "?parents&children&access"),
      vb = vb
    )
  
  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    if (component == "children") {
      r <- g$children
      r[[1]] # Extract from list
    } else if (component == "parents") {
      r <- g$adults # Extract from list
      r[[1]]
    } else {
      g
    }
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
