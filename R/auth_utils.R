# Internal helpers for authentication and credential management

#' @noRd
CREDENTIAL_ENV_VARS <- c(
  email = "DATABRARY_LOGIN",
  password = "DATABRARY_PASSWORD",
  client_id = "DATABRARY_CLIENT_ID",
  client_secret = "DATABRARY_CLIENT_SECRET"
)

#' @noRd
is_missing_string <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(TRUE)
  }
  value <- x[[1]]
  if (is.na(value)) {
    return(TRUE)
  }
  if (!is.character(value)) {
    return(FALSE)
  }
  trimmed <- trimws(value)
  identical(trimmed, "")
}

#' @noRd
try_keyring_get <- function(service, username, vb = FALSE) {
  if (!keyring::has_keyring_support()) {
    return(NULL)
  }
  if (is_missing_string(username)) {
    return(NULL)
  }
  result <- try(keyring::key_get(service = service, username = username), silent = TRUE)
  if (inherits(result, "try-error")) {
    if (vb) {
      message("No keyring entry for service='", service, "' and username='", username, "'.")
    }
    return(NULL)
  }
  if (is_missing_string(result)) {
    return(NULL)
  }
  result
}

#' @noRd
store_keyring_value <- function(service, username, value, vb = FALSE) {
  if (!keyring::has_keyring_support()) {
    return(FALSE)
  }
  if (is_missing_string(value) || is_missing_string(username)) {
    return(FALSE)
  }
  outcome <- try(keyring::key_set_with_value(
    service = service,
    username = username,
    password = value
  ), silent = TRUE)
  if (inherits(outcome, "try-error")) {
    if (vb) {
      message("Unable to store keyring entry for service='", service, "' and username='", username, "'.")
    }
    return(FALSE)
  }
  if (vb) {
    message("Stored credentials in keyring service='", service, "'.")
  }
  TRUE
}

#' @noRd
resolve_credential_value <- function(label,
                                     value,
                                     prompt_label,
                                     service,
                                     username = NULL,
                                     overwrite,
                                     vb) {
  if (!is_missing_string(value)) {
    assertthat::assert_that(assertthat::is.string(value))
    return(value)
  }

  # Check environment variable using the static map
  env_var_name <- CREDENTIAL_ENV_VARS[[label]]
  env_value <- Sys.getenv(env_var_name, NA_character_)
  if (!is.na(env_value) && nzchar(env_value)) {
    return(env_value)
  }

  # For email, skip keyring lookup since email is the identifier used for other keyring lookups
  # Only do keyring lookup for client_id and other credentials that are actually stored
  if (!is_missing_string(username) && !overwrite && label != "email") {
    stored <- try_keyring_get(service = service, username = username, vb = vb)
    if (!is.null(stored)) {
      return(stored)
    }
  }

  message("Please enter your ", prompt_label, ".")
  readline(prompt = paste0(prompt_label, ": "))
}

#' @noRd
resolve_secret_value <- function(label,
                                 value,
                                 prompt_label,
                                 service,
                                 username,
                                 overwrite,
                                 vb) {
  if (!is_missing_string(value)) {
    assertthat::assert_that(assertthat::is.string(value))
    return(value)
  }

  # Check environment variable using the static map
  env_var_name <- CREDENTIAL_ENV_VARS[[label]]
  env_value <- Sys.getenv(env_var_name, NA_character_)
  if (!is.na(env_value) && nzchar(env_value)) {
    return(env_value)
  }

  if (!overwrite) {
    recovered <- try_keyring_get(service = service, username = username, vb = vb)
    if (!is.null(recovered)) {
      return(recovered)
    }
  }

  getPass::getPass(paste0("Please enter your ", prompt_label, " "))
}
