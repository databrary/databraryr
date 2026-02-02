#' Set base request defaults for Databrary API.
#'
#' Creates an `httr2` request with the package's default options, including
#' base URL, user agent, Accept header, and timeout tuned for the Django API.
#'
#' @inheritParams options_params
#' @param with_token Should the request include an OAuth2 `Authorization` header?
#'   Defaults to `TRUE` since all API calls now require authentication.
#' @param refresh When `with_token = TRUE`, determines whether to refresh the
#'   cached token if it is near expiry. Defaults to `TRUE`.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#'
#' @returns An `httr2_request` object configured for the Databrary API.
#'
#' @examples
#' make_default_request()
#' @export
make_default_request <- function(with_token = TRUE,
                                 refresh = TRUE,
                                 vb = options::opt("vb")) {
  assertthat::assert_that(is.logical(with_token), length(with_token) == 1)
  assertthat::assert_that(is.logical(refresh), length(refresh) == 1)
  assertthat::assert_that(is.logical(vb), length(vb) == 1)

  req <- httr2::request(DATABRARY_BASE_URL) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_retry(max_tries = RETRY_LIMIT) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_timeout(REQUEST_TIMEOUT)

  if (!isTRUE(with_token)) {
    return(req)
  }

  token <- if (isTRUE(refresh)) {
    bundle <- ensure_valid_token(refresh = TRUE, vb = vb)
    bundle$access_token
  } else {
    require_access_token()
  }

  httr2::req_headers(req, Authorization = paste("Bearer", token))
}