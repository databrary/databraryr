#' Logs in to Databrary.org.
#'
#' @param email Databrary account ID (email).
#' @param login.url URL for the login API.
#' @param return.reponse A Boolean value. If TRUE returns results of GET
#' @param save.session A Boolean value. If TRUE saves the session cookie.
#' @param stored.credentials A Boolean value. If TRUE loads login credentials from credentials file.
#' @param credentials.file Path to stored credentials file if it exists.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return Status code if login is successful
#' @examples
#' login_db()
#' @export
login_db <- function(email = NULL,
                     login.url = "/api/user/login",
                     return.response = FALSE, save.session = TRUE,
                     stored.credentials = FALSE,
                     system.credentials = TRUE,
                     credentials.file = "~/api-keys/json/databrary-keys.json",
                     vb = FALSE ) {

  # Check parameters
  if (length(email) > 1) {
    stop("email must have length == 1.")
  }
  if (is.numeric(email) || is.logical(email)) {
    stop("email must be a string.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must have logical value.")
  }

  # Access (possibly stored) credentials
  if (stored.credentials) {
    email <- jsonlite::fromJSON(credentials.file)$email
    password <- jsonlite::fromJSON(credentials.file)$pw
  } else if (system.credentials) {
    if (!(exists('email')) || (is.null(email))) {
      message("Please enter your Databrary user ID (email).")
      email <- readline(prompt="Email: ")
    }
    kl <- keyring::key_list(service = "databrary")
    if (exists('kl') && is.data.frame(kl)) {
      if (email %in% kl$username) {
        password <- keyring::key_get(service = "databrary",
                                     username = email)
      } else {
        if (vb) message(paste0("No password for user: ", email, "\n"))
        return(FALSE)
      }
    } else {
      if (vb) message(paste0("No stored credentials for user: ", email, "\n"))
      return(FALSE)
    }
  } else {
    email = rstudioapi::askForPassword("Please enter your Databrary account ID (email): ")
    password = rstudioapi::askForPassword("Please enter your Databrary password: ")
  }

  # Assemble URL, POST(), and handle response
  r <- httr::POST(paste0("https://nyu.databrary.org/api/user/login"),
                  body = list(email=email, password=password))
  rm(email, password)

  if (httr::status_code(r) == 200){
    if (save.session) {
      if (vb) message("Saving session cookie.")
      databrary.SESSION <- httr::cookies(r)$value
      save(databrary.SESSION, file = ".databrary.RData")
    } else {
      rm(databrary.SESSION)
    }
    message(paste("Login successful."))
    return(TRUE)
  } else {
    message(paste0('Login Failed, HTTP status ', httr::status_code(r), '\n'))
    return(FALSE)
  }
}
