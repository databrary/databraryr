#' Log In To Databrary.org.
#'
#' @param email Databrary account email address.
#' @param password Databrary password (not recommended as it will displayed
#' as you type)
#' @param client_id OAuth2 client identifier.
#' @param client_secret OAuth2 client secret.
#' @param store A boolean value. If TRUE store/retrieve credentials from the
#'   system keyring/keychain.
#' @param overwrite A boolean value. If TRUE and store is TRUE, overwrite/
#'   update stored credentials in keyring/keychain.
#' @param service A character label for stored credentials in the keyring.
#'   Default is `org.databrary.databraryr`.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#'
#' @returns Logical value indicating whether log in is successful or not.
#'
#' @examplesIf interactive()
#' login_db() # Queries user for email and password interactively.
#' @examples
#' \donttest{
#' \dontrun{
#'# The following shows how to use credentials that have been stored previously.
#'
#' login_db(email = "you@provider.com", store = TRUE)
#'
#' }
#' }
#' @export
login_db <- function(email = NULL,
                     password = NULL,
                     client_id = NULL,
                     client_secret = NULL,
                     store = FALSE,
                     overwrite = FALSE,
                     service = KEYRING_SERVICE,
                     vb = options::opt("vb")) {
  assertthat::assert_that(length(store) == 1, is.logical(store))
  validate_flag(overwrite, "overwrite")
  validate_flag(vb, "vb")
  assertthat::assert_that(length(service) == 1, is.character(service))

  # If the user wants to store or use their stored credentials,
  # check for keyring support
  if (store) {
    assertthat::assert_that(keyring::has_keyring_support(),
                            msg = "No keyring support; please use store=FALSE")
  }

  email_value <- resolve_credential_value(
    label = "email",
    value = email,
    prompt_label = "Databrary user ID (email)",
    service = service,
    overwrite = overwrite,
    vb = vb
  )

  password_value <- resolve_secret_value(
    label = "password",
    value = password,
    prompt_label = "Databrary password",
    service = service,
    username = paste0(email_value, "::password"),
    overwrite = overwrite,
    vb = vb
  )

  client_id_value <- resolve_credential_value(
    label = "client_id",
    value = client_id,
    prompt_label = "OAuth client ID",
    service = service,
    username = paste0(email_value, "::client_id"),
    overwrite = overwrite,
    vb = vb
  )

  client_secret_value <- resolve_secret_value(
    label = "client_secret",
    value = client_secret,
    prompt_label = "OAuth client secret",
    service = service,
    username = paste0(email_value, "::client_secret"),
    overwrite = overwrite,
    vb = vb
  )

  token <- oauth_password_grant(
    username = email_value,
    password = password_value,
    client_id = client_id_value,
    client_secret = client_secret_value,
    vb = vb
  )

  if (is.null(token)) {
    if (vb) message("Login failed; see previous messages for details.")
    return(FALSE)
  }

  set_token_bundle(
    access_token = token$access_token,
    refresh_token = token$refresh_token,
    expires_in = token$expires_in,
    issued_at = Sys.time(),
    client_id = client_id_value,
    client_secret = client_secret_value,
    username = email_value
  )

  if (store) {
    store_keyring_value(
      service = service, username = paste0(email_value, "::password"),
      value = password_value, vb = vb
    )
    store_keyring_value(
      service = service, username = paste0(email_value, "::client_id"),
      value = client_id_value, vb = vb
    )
    store_keyring_value(
      service = service, username = paste0(email_value, "::client_secret"),
      value = client_secret_value, vb = vb
    )
  }

  if (vb) message("Login successful.")
  TRUE
}
