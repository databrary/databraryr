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

  constants_url <- "https://nyu.databrary.org/api/constants"
  if (vb) {
    message(paste0("Sending GET to ", constants_url))
  }
  g <- httr::GET(constants_url)
  if (httr::status_code(g) == 200) {
    g_content <- httr::content(g, 'text', encoding = 'UTF-8')
    return(jsonlite::fromJSON(g_content))
  } else if (vb) {
    message(paste0( 'Download Failed, HTTP status ', httr::status_code(g), '\n'))
  }
}
