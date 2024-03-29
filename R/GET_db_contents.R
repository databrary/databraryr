#' Queries the Databrary API with an HTML GET command
#'
#' @description `GET_db_contents` is a helper function that retrieves
#' information from the 'Databrary.org' API in a structured way.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated and may be removed in a future version.
#'
#' @param base_URL Base URL for API call. Default is https://nyu.databrary.org.
#' @param URL_components Other components of API URL usually assembled using a paste0() command.
#' @param convert_JSON A Boolean value. If TRUE, convert JSON to a data frame. Default is TRUE.
#' @param vb A Boolean value. If TRUE provides verbose output.
#'
#' @returns Output from GET command with the specified parameters, usually a string or a data frame.
#'
#' @examples
#' \donttest{
#' GET_db_contents() # Returns a list with data from volume 1.
#' }
#'
#' @export
GET_db_contents <- function(base_URL = 'https://nyu.databrary.org',
                            URL_components = '/api/volume/1',
                            convert_JSON = TRUE,
                            vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(base_URL) == 1)
  assertthat::assert_that(is.character(base_URL))
  
  assertthat::assert_that(length(URL_components) == 1)
  assertthat::assert_that(is.character(URL_components))
  
  assertthat::assert_that(length(convert_JSON) == 1)
  assertthat::assert_that(is.logical(convert_JSON))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  URL <- paste0(base_URL, URL_components)
  if (is.null(URL)) {
    stop('URL contains null value.')
  }
  if (vb) {
    message(paste0("Sending GET to ", URL))
  }
  g <- httr::GET(URL, httr::progress())
  
  if (httr::status_code(g) == 200) {
    if (vb) {
      message('Successful HTML GET query.')
    }
    # Some routes do not report content-type  though content is well-structured
    if ("content-type" %in% names(g$headers)) {
      if (vb)
        message(paste0('Content type is ', g$headers$`content-type`), '.')
      if (g$headers$`content-type` %in% c("image/png", "image/jpeg")) {
        if (vb)
          message('Returning image content.')
        httr::content(g, as = 'raw')
      } else if (stringr::str_detect(g$headers$`content-type`, "application/json")) {
        g_content <- httr::content(g, 'text', encoding = 'UTF-8')
        if (convert_JSON) {
          if (vb)
            message('Converting JSON.')
          jsonlite::fromJSON(g_content)
        } else {
          if (vb)
            message('Returning unconverted content.')
          g_content
        }
      } else if (stringr::str_detect(g$headers$`content-type`, "text/csv")) {
        httr::content(g, as = 'parsed', col_types = list(.default = "c"))
      } else if (stringr::str_detect(g$headers$`content-type`, "text/html")) {
        httr::content(g, 'text', encoding = 'UTF-8')
        # Zip file
      } else if (stringr::str_detect(g$headers$`content-type`, "application/zip")) {
        httr::content(g, as = 'raw')
      }
    } else {
      if (vb)
        message(paste0('Content type is unknown. Assuming JSON text.'))
      g_content <- httr::content(g, 'text', encoding = 'UTF-8')
      if (convert_JSON) {
        if (vb)
          message('Converting JSON.')
        jsonlite::fromJSON(g_content)
      } else {
        if (vb)
          message('Returning unconverted content.')
        g_content
      }
    }
  } else if (vb) {
    message(paste0('Download failed; HTTP status ', httr::status_code(g), '\n'))
  }
}
