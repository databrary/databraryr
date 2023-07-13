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
  if (!is.character(logout.url)) {
    stop("logout.url must be a character string.")    
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  r <- httr::POST("http://nyu.databrary.org/api/user/logout")
  if (httr::status_code(r) == 200) {
    if (vb) message('Logout Successful.')
    if (file.exists(".databrary.RData")) file.remove(".databrary.RData")
    httr::set_config(httr::config(cookie = NULL))
    return(TRUE)
  } else {
    if (vb) message(paste('Logout Failed, HTTP status ', httr::status_code(r), '\n', sep="" ))
    return(FALSE)
  }
}
