#' Logs in to Databrary.org.
#'
#' @param login.url URL for the login API.
#' @param return.reponse A Boolean value. If TRUE returns results of GET
#' @param save.session A Boolean value. If TRUE saves the session cookie.
#' @param stored.credentials A Boolean value. If TRUE loads login credentials from credentials file.
#' @param credentials.file Path to stored credentials file if it exists.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Status code if successful.
#' @examples
#' login_db()
login_db <- function(login.url = "/api/user/login",
                     return.response = FALSE, save.session = TRUE,
                     stored.credentials = TRUE,
                     credentials.file = "~/api-keys/json/databrary-keys.json",
                     vb = FALSE ) {
  # Logs in to Databrary using stored credentials (default) or email & password provided at command line.
  #
  # Args:
  #  login.url: URL for login API. Default is "/api/user/login".
  #  return.response: Flag specifying whether to return the HTTP response. Default is FALSE.
  #  save.session: Flag specifying whether to save session info in a cookie. Default is TRUE.
  #  stored.credentials: Flag specifying whether to look for stored credientials. Default is TRUE.
  #  credentials.file: File path where credentials are stored. Default is "~/api-keys/json/databrary-keys.json".
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.

  if (!exists("databrary_config_status")) {
    config_db(vb = vb)
  }

  if (stored.credentials) {
    email <- jsonlite::fromJSON(credentials.file)$email
    password <- jsonlite::fromJSON(credentials.file)$pw
  } else {
    # TODO(someone): Make login more secure.
    email <- readline(prompt="Email: ")
    password <- readline(prompt="Password: ")
    cat("\014")   # clear console
  }

  r <- httr::POST(paste0(databrary.url, login.url),
                  body = list(email=email, password=password))
  # TODO(someone): Make login more secure.
  rm(email, password)

  if (httr::status_code(r) == 200){
    if (vb) cat( 'Login Successful.\n' )
    if (save.session){
      databrary.SESSION <- httr::cookies(r)$value
      save(databrary.SESSION, file = ".databrary.RData")
    } else {
      rm(databrary.SESSION)
    }
  } else if (vb) cat(paste('Login Failed, HTTP status ', httr::status_code(r), '\n', sep="" ) )

  if (return.response) return(r)
}
