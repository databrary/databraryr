#' Retrieve metadata about the authenticated Databrary user.
#'
#' Calls the Django `/oauth2/test/` endpoint to report the current authentication
#' method and user profile. Requires a valid OAuth2 access token acquired via
#' `login_db()`.
#'
#' @inheritParams options_params
#' @param refresh Whether to attempt automatic token refresh when the current
#'   access token is expired. Defaults to `TRUE`.
#'
#' @returns A list containing `auth_method` and `user` fields (both lists) or
#'   `NULL` if the request fails due to lack of authentication.
#'
#' @examples
#' \\dontrun{
#' login_db()
#' whoami()
#' }
#' @export
whoami <- function(refresh = TRUE, vb = options::opt("vb")) {
  assertthat::assert_that(is.logical(refresh), length(refresh) == 1)
  assertthat::assert_that(is.logical(vb), length(vb) == 1)

  req <- tryCatch(
    make_default_request(refresh = refresh, vb = vb),
    error = function(err) {
      if (vb) message("Authentication required: ", conditionMessage(err))
      NULL
    }
  )

  if (is.null(req)) {
    return(NULL)
  }

  resp <- tryCatch(
    httr2::req_url(req, OAUTH_TEST_URL) |>
      httr2::req_perform(),
    error = function(err) {
      if (vb) message("whoami request failed: ", conditionMessage(err))
      NULL
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    if (vb) message(httr2_error_message(resp))
    return(NULL)
  }

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

