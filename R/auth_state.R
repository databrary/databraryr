# Token state management -------------------------------------------------------

.databrary_token_env <- new.env(parent = emptyenv())

#' @noRd
set_token_bundle <- function(access_token,
                             refresh_token = NULL,
                             expires_in = NULL,
                             issued_at = Sys.time(),
                             client_id = NULL,
                             client_secret = NULL,
                             username = NULL) {
  assertthat::assert_that(assertthat::is.string(access_token))
  .databrary_token_env$access_token <- access_token
  .databrary_token_env$refresh_token <- if (is_missing_string(refresh_token)) NULL else refresh_token
  .databrary_token_env$issued_at <- issued_at
  if (is.null(expires_in)) {
    .databrary_token_env$expires_at <- NULL
  } else {
    assertthat::assert_that(is.numeric(expires_in), length(expires_in) == 1)
    .databrary_token_env$expires_at <- issued_at + as.difftime(as.numeric(expires_in), units = "secs")
  }
  .databrary_token_env$client_id <- if (is_missing_string(client_id)) NULL else client_id
  .databrary_token_env$client_secret <- if (is_missing_string(client_secret)) NULL else client_secret
  .databrary_token_env$username <- if (is_missing_string(username)) NULL else username
  invisible(.databrary_token_env)
}

#' @noRd
get_token_bundle <- function() {
  if (!is.null(.databrary_token_env$access_token)) {
    return(list(
      access_token = .databrary_token_env$access_token,
      refresh_token = .databrary_token_env$refresh_token,
      expires_at = .databrary_token_env$expires_at,
      issued_at = .databrary_token_env$issued_at,
      client_id = .databrary_token_env$client_id,
      client_secret = .databrary_token_env$client_secret,
      username = .databrary_token_env$username
    ))
  }
  NULL
}

#' @noRd
clear_token_bundle <- function() {
  rm(list = ls(.databrary_token_env), envir = .databrary_token_env)
  invisible(NULL)
}

#' @noRd
token_should_refresh <- function() {
  bundle <- get_token_bundle()
  if (is.null(bundle)) {
    return(FALSE)
  }
  expires_at <- bundle$expires_at
  if (is.null(expires_at)) {
    return(FALSE)
  }
  now <- Sys.time()
  now >= (expires_at - as.difftime(30, units = "secs"))
}

#' @noRd
require_access_token <- function() {
  bundle <- get_token_bundle()
  if (is.null(bundle) || is_missing_string(bundle$access_token)) {
    stop("No access token available. Please call login_db() first.", call. = FALSE)
  }
  bundle$access_token
}
