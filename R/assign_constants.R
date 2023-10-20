#' Download Databrary Constants From API.
#' 
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with the constants.
#' @examples
#' \donttest{
#' assign_constants()
#' }
#' @export
assign_constants <- function(vb = FALSE) {
  # Check parameter
  assertthat::assert_that(is.logical(vb))

  r <- GET_db_contents(URL_components = '/api/constants', vb=vb)
  r
}
