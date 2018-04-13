#' Logs out of Databrary.org.
#'
#' @param logout.url URL for the login API.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' logout_db()
logout_db <- function(logout.url = "/api/user/logout", vb = TRUE){
  # if (!exists("databrary_config_status")) {
  #   databraryapi::config_db(vb = vb)
  # }

  r <- httr::POST("http://nyu.databrary.org/api/user/logout")
  if (httr::status_code(r) == 200) {
    if (vb) message('Logout Successful.')
    if (file.exists(".databrary.RData")) file.remove(".databrary.RData")
    httr::set_config(httr::config(cookie = NULL))
  } else if (vb) message(paste('Logout Failed, HTTP status ', httr::status_code(r), '\n', sep="" ))
}
