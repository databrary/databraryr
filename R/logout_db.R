#' Log Out of Databrary.org.
#'
#' @param logout.url URL for the login API.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns Status code if successful.
#' @examples
#' logout_db()
#' @export
logout_db <- function(logout.url = "/api/user/logout", vb = TRUE){
  
  # Check parameters
  assertthat::assert_that(length(logout.url) == 1)
  assertthat::assert_that(is.character(logout.url))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  url <- paste0("http://nyu.databrary.org", logout.url)
  r <- httr::POST(url)

  if (httr::status_code(r) == 200) {
    if (vb) message('Logout Successful.')
    httr::set_config(httr::config(cookie = NULL))
    return(TRUE)
  } else {
    if (vb) message(paste('Logout Failed, HTTP status ', httr::status_code(r), '\n', sep="" ))
    return(FALSE)
  }
}
