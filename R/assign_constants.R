#' Downloads Databrary contants from API.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the constants.
#' @examples
#' assign_constants()
#' @export
assign_constants <- function(vb = FALSE) {
  # Error checking
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  r <- GET_db_contents(URL_components = '/api/constants', vb=vb)
  r
}
