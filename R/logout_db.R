#' Logs out of Databrary.org.
#'
#' @param logout.url URL for the login API.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' logout_db()
logout_db <- function(logout.url = "/api/user/logout", vb = TRUE){
  if (!exists("databrary_config_status")) {
    databraryapi::config_db(vb = vb)
  }

  r <- httr::POST(paste0(databrary.url, logout.url))
  if (httr::status_code(r) == 200){
    if (vb) message('Logout Successful.')
    if (file.exists(".databrary.RData")) file.remove(".databrary.RData")
    if (exists('databrary_config_status')) rm(databrary_config_status, envir = .GlobalEnv)
    if (exists('databrary.url')) rm(databrary.url, envir = .GlobalEnv)
    if (exists('system.credentials')) rm(system.credentials, envir = .GlobalEnv)
    if (exists('vol.api.url')) rm(vol.api.url, envir = .GlobalEnv)
    if (exists('logged.in')) rm(logged.in, envir = .GlobalEnv)
  } else if (vb) message(paste('Logout Failed, HTTP status ', httr::status_code(r), '\n', sep="" ))
}
