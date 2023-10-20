#' List Data For A Party (person or institution).
#'
#' @param party_id Target volume number. Default is volume 8 (NYU).
#' @param component Which data to return 'children', 'parents', or 'all'. Default is all.
#' @param vb A Boolean value. If TRUE provides verbose output. Default is FALSE.
#' @returns A data frame with information about a party (person or institution).
#' @examples
#' \donttest{
#' list_party() # Default is New York University (party 8)
#' #' }
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
  
  if (length(party_id) > 1) {
    stop("party_id must have length == 1.")
  }
  if (!is.numeric(party_id)) {
    stop("party_id must be an integer.")
  }
  if (party_id < 0) {
    stop("party_id must be > 0.")
  }
  if (length(component) > 1) {
    stop("component must have length == 1.")
  }
  if (!is.character(component)) {
    stop("component must be a character string.")
  }
  if (!(component %in% c('children', 'parents', 'all'))) {
    stop("component must be one of 'children', 'parents', or 'all'.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be a Boolean.")
  }
  
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
