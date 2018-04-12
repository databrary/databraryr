#' Downloads Databrary contants from API.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the constants.
#' @examples
#' assign_constants()
assign_constants <- function(vb = FALSE) {
  constants.url <- "https://nyu.databrary.org/api/constants"
  if (vb) {
    message(paste0("Sending GET to ", constants.url))
  }
  g <- httr::GET(constants.url)
  if (httr::status_code(g) == 200) {
    g.content <- httr::content(g, 'text', encoding = 'UTF-8')
    return(jsonlite::fromJSON(g.content))
  } else if (vb) {
    message(paste0( 'Download Failed, HTTP status ', httr::status_code(g), '\n'))
  }
}
