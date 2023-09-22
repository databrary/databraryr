#' Log In To Databrary.org.
#'
#' @param email Databrary account email address.
#' @param password Databrary password (not recommended as it will displayed as you type)
#' @param store A boolean value. If TRUE store/retrieve credentials from the system keyring/keychain.
#' @param overwrite A boolean value. If TRUE and store is TRUE, overwrite/ update stored credentials in keyring/keychain.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns Logical value indicating whether log in is successful or not.
#' @examples
#' \dontrun{
#' login_db()
#' }
#' @export
login_db <- function(email = NULL,
                     password = NULL,
                     store = FALSE,
                     overwrite = FALSE,
                     vb = FALSE,
                     SERVICE = "databrary") {
  
  # Check parameters
  assertthat::assert_that(is.logical(store))
  assertthat::assert_that(is.logical(overwrite))
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(is.character(SERVICE))
  
  # If the user wants to store or use their stored credentials, check for keyring support
  if (store) {
    assertthat::assert_that(keyring::has_keyring_support(), msg = "No keyring support; please use store=FALSE")
  }
  
  # Check or get email
  if (!is.null(email)) {
    assertthat::assert_that(assertthat::is.string(email))
  } else {
    message("Please enter your Databrary user ID (email).")
    email <- readline(prompt = "Email: ")
  }
  
  do_collect_password <- TRUE
  
  if (!is.null(password)) {
    assertthat::assert_that(assertthat::is.string(password))
    do_collect_password <- FALSE
  }
  
  # SERVICE <- "org.databrary.databraryr"
  # SERVICE <- "databrary" # Temporary to test existing keyring store with this service name.
  
  # If the user wants to store or use their stored credentials and
  # doesn't provide a password
  if (store && is.null(password) && !overwrite) {
    if (vb) message("Retrieving password for service='", SERVICE, "' from keyring.")
    kl <- keyring::key_list(service = SERVICE)
    # Make sure our service is in the keyring
    if (exists('kl') && is.data.frame(kl)) {
      # If it is under the email entered, keep it to try later and not collect it here
      password <-
        try(keyring::key_get(service = SERVICE, username = email),
            silent = TRUE)
      if ("try-error" %in% class(password)) {
        do_collect_password <- TRUE
        if (vb) message("No password found in keyring for service='", SERVICE, ".")
      } else {
        do_collect_password <- FALSE
        if (vb) message("Password retrieved from keyring.")
      }
    } else {
      if (vb) message("Error retrieving keyring data for service='", SERVICE, "'.")
    }
  }
  
  # If we need to, securely collect the password
  if (do_collect_password) {
    password <-
      getPass::getPass("Please enter your Databrary password ")
  }
  
  is_login_successful <- FALSE
  
  r <-
    httr::POST(
      paste0("https://nyu.databrary.org/api/user/login"),
      body = list(email = email, password = password)
    )
  
  if (httr::status_code(r) == 200) {
    is_login_successful <- TRUE
  }
  
  # If the username/password was successful and the user wanted to store their credentials
  # Store them in the keyring
  if (is_login_successful) {
    if (store && (do_collect_password || overwrite)) {
      keyring::key_set_with_value(service = SERVICE,
                                  username = email,
                                  password = password)
      if (vb)
        message(paste0("Login successful; password stored in keyring/keychain"))
    } else {
      if (vb)
        message(paste("Login successful."))
    }
    return(TRUE)
  }
  
  if (store) {
    if (vb)
      message(
        paste0(
          'Login failed; nothing stored in keyring; HTTP status ',
          httr::status_code(r),
          '\n'
        )
      )
  } else {
    if (vb)
      message(paste0('Login failed; HTTP status ', httr::status_code(r), '\n'))
  }
  
  return(FALSE)
}
