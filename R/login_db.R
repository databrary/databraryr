#' Logs In to Databrary.org.
#'
#' @param email Databrary account ID (email).
#' @param login_url URL for the login API.
#' @param return_response A Boolean value. If TRUE returns results of GET
#' @param save_session A Boolean value. If TRUE saves the session cookie.
#' @param stored_credentials A Boolean value. If TRUE loads login credentials from credentials file.
#' @param system_credentials A Boolean value. If TRUE loads login credentials using the system level file accessed by the `keyring` package.
#' @param credentials_file Path to stored credentials file if it exists.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns Logical value indicating whether login is successful or not.
#' @examples
#' login_db()
#' @export
login_db <- function(email = NULL,
                     login_url = "/api/user/login",
                     return_response = FALSE,
                     save_session = TRUE,
                     stored_credentials = FALSE,
                     system_credentials = TRUE,
                     credentials_file = file.path(path.expand("~"), 
                                                  "api-keys", "json", 
                                                  "databrary-keys.json"),
                     vb = FALSE) {
  # Check parameters
  if (length(email) > 1) {
    stop("email must have length == 1.")
  }
  if (is.numeric(email) || is.logical(email)) {
    stop("email must be a string.")
  }
  if (!is.character(login_url)) {
    stop("login_url must be a string.")
  }
  if (length(login_url) > 1) {
    stop("`login_url` must have length == 1.")
  }
  if (length(return_response) > 1) {
    stop("return_response must have length == 1.")
  }
  if (!is.logical(return_response)) {
    stop("return_response must have logical value.")
  }
  if (length(save_session) > 1) {
    stop("save_session must have length == 1.")
  }
  if (!is.logical(save_session)) {
    stop("`save_session` must have logical value.")
  }
  if (length(stored_credentials) > 1) {
    stop("stored_credentials must have length == 1.")
  }
  if (!is.logical(stored_credentials)) {
    stop("stored_credentials must have logical value.")
  }
  if (length(system_credentials) > 1) {
    stop("system_credentials must have length == 1.")
  }
  if (!is.logical(system_credentials)) {
    stop("system_credentials must have logical value.")
  }
  if (!is.character(credentials_file)) {
    stop("credentials_file must be a string.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must have logical value.")
  }
  
  # Access (possibly stored) credentials
  if (stored_credentials) {
    email <- jsonlite::fromJSON(credentials_file)$email
    password <- jsonlite::fromJSON(credentials_file)$pw
  } else if (system_credentials) {
    if (!(exists('email')) || (is.null(email))) {
      message("Please enter your Databrary user ID (email).")
      email <- readline(prompt = "Email: ")
    }
    kl <- keyring::key_list(service = "databrary")
    if (exists('kl') && is.data.frame(kl)) {
      if (email %in% kl$username) {
        password <- keyring::key_get(service = "databrary",
                                     username = email)
      } else {
        if (vb)
          message(paste0("No password for user: ", email, "\n"))
        return(FALSE)
      }
    } else {
      if (vb)
        message(paste0("No stored credentials for user: ", email, "\n"))
      return(FALSE)
    }
  } else {
    email = rstudioapi::askForPassword("Please enter your Databrary account ID (email): ")
    password = rstudioapi::askForPassword("Please enter your Databrary password: ")
  }
  
  # Assemble URL, POST(), and handle response
  r <-
    httr::POST(
      paste0("https://nyu.databrary.org/api/user/login"),
      body = list(email = email, password = password)
    )
  # Delete credentials from R environment
  rm(email, password)
  
  if (httr::status_code(r) == 200) {
    if (save_session) {
      if (vb)
        message("Saving session cookie.")
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
