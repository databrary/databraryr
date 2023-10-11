#' Log Out of Databrary.org.
#'
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns Status code if successful.
#' @examples
#' \dontrun{
#' logout_db()
#' }
#' @export
logout_db <- function(vb = TRUE){
  
  # Check parameters
  assertthat::assert_that(is.logical(vb))
  
  url <- "https://nyu.databrary.org/api/user/logout"
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
