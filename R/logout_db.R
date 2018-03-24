#' Logs out of Databrary.org.
#'
#' @param logout.url URL for the login API.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' logout_db()
logout_db <- function(logout.url = "/api/user/logout",
                      return.response = FALSE, vb = TRUE){
  if (!exists("databrary_config_status")) {
    config_db(vb = vb)
  }

  r <- POST(paste0(databrary.url, logout.url))

  if (status_code(r) == 200){
    if (vb) cat( 'Logout Successful.\n' )
    if (file.exists(".databrary.RData")) file.remove(".databrary.RData")
  } else if (vb) cat( paste('Logout Failed, HTTP status ', status_code(r), '\n', sep="" ))
  if (return.response) return(r)
}
