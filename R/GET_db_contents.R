#' Queries the Databrary API with an HTML GET command
#'
#' @param base_URL Base URL for API call. Default is https://nyu.databrary.org/api/.
#' @param URL_components Other components of API URL usually assembled using a paste0() command.
#' @param convert_JSON A Boolean value. If TRUE, convert JSON to a data frame. Default is TRUE.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' GET_db_contents()
#' @export
GET_db_contents <- function(base_URL='https://nyu.databrary.org/api/',
                            URL_components,
                            convert_JSON = TRUE, vb = FALSE) {
  URL <- paste0(base_URL, URL_components)
  if (is.null(URL)) {
    stop('URL contains null value.')
  }
  if (vb) {
    message(paste0("Sending GET to ", URL))
  }
  g <- httr::GET(URL)
  if (httr::status_code(g) == 200) {
    if (vb) {
      message('Successful HTML GET query.')
      message(paste0('Content type is ', g$headers$`content-type`), '.')
    }
    g_content <- httr::content(g, 'text', encoding = 'UTF-8')
    if (convert_JSON) {
      if (vb) message('Converting JSON to data frame.')
      return(jsonlite::fromJSON(g_content))
    } else {
      return(g_content)
    }
  } else if (vb) {
    message(paste0('Download failed; HTTP status ', httr::status_code(g), '\n'))
  }
}
