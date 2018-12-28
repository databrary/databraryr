#' Logs out of Databrary.org.
#'
#' @param logout.url URL for the login API.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' logout_db()
#' @export
logout_db <- function(vb = TRUE){
  # if (!exists("databrary_config_status")) {
  #   databraryapi::config_db(vb = vb)
  # }
  # Check parameters
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  logout.url = "/api/user/logout"
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
