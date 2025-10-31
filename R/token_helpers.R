# Token-aware request helpers -------------------------------------------------

#' @noRd
ensure_valid_token <- function(refresh = TRUE,
                               client_id = NULL,
                               client_secret = NULL,
                               vb = FALSE) {
  bundle <- get_token_bundle()
  if (is.null(bundle)) {
    stop("No OAuth token available; call login_db() first.", call. = FALSE)
  }

  if (!token_should_refresh()) {
    return(bundle)
  }

  if (!refresh) {
    stop("Access token expired and refresh disabled.", call. = FALSE)
  }

  refresh_token <- bundle$refresh_token
  if (is_missing_string(refresh_token)) {
    stop("Access token expired and no refresh token available.", call. = FALSE)
  }

  refresh_client_id <- if (is_missing_string(client_id)) bundle$client_id else client_id
  refresh_client_secret <- if (is_missing_string(client_secret)) bundle$client_secret else client_secret

  refreshed <- oauth_refresh_grant(
    refresh_token = refresh_token,
    client_id = refresh_client_id,
    client_secret = refresh_client_secret,
    vb = vb
  )

  if (is.null(refreshed)) {
    clear_token_bundle()
    stop("Token refresh failed; please re-authenticate with login_db().", call. = FALSE)
  }

  set_token_bundle(
    access_token = refreshed$access_token,
    refresh_token = refreshed$refresh_token,
    expires_in = refreshed$expires_in,
    issued_at = Sys.time(),
    client_id = refresh_client_id,
    client_secret = refresh_client_secret,
    username = bundle$username
  )

  get_token_bundle()
}
