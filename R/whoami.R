#' Retrieve metadata about the authenticated Databrary user.
#'
#' Calls the Django `/oauth2/test/` endpoint to report the current authentication
#' method and user profile. Requires a valid OAuth2 access token acquired via
#' `login_db()`.
#'
#' @inheritParams options_params
#'
#' @param refresh Whether to attempt automatic token refresh when the current
#'   access token is expired. Defaults to `TRUE`.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#'
#' @returns A list containing `auth_method` and `user` fields (both lists) or
#'   `NULL` if the request fails due to lack of authentication.
#'
#' @examples
#' \dontrun{
#' login_db()
#' whoami()
#' }
#' @export
whoami <- function(refresh = TRUE,
                   vb = options::opt("vb")) {
  validate_flag(refresh, "refresh")
  validate_flag(vb, "vb")

  req <- tryCatch(
    make_default_request(refresh = refresh, vb = vb),
    error = function(err) {
      if (vb)
        message("Authentication required: ", conditionMessage(err))
      NULL
    }
  )

  if (is.null(req)) {
    return(NULL)
  }

  resp <- tryCatch(
    req |>
      httr2::req_url(OAUTH_TEST_URL) |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_perform(),
    error = function(err) {
      if (vb) {
        message("whoami request failed: ", conditionMessage(err))
        message("whoami -> request url: ", OAUTH_TEST_URL)
        bundle <- get_token_bundle()
        token <- if (is.null(bundle) || is.null(bundle$access_token)) {
          ""
        } else {
          bundle$access_token
        }
        auth_desc <- if (nzchar(token)) {
          paste0("Bearer ", substr(token, 1L, 8L), "...")
        } else {
          "<missing>"
        }
        message("whoami -> authorization header: ", auth_desc)
      }
      NULL
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    if (vb)
      message(httr2_error_message(resp))
    return(NULL)
  }

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}
