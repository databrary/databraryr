#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download Information About a User on Databrary as JSON
#'
#' @param user_id An integer. The user number to retrieve information about.
#' @param rq An `httr2`-style request object. If NULL, then a new request will
#' be generated using `make_default_request()`.
#'
#' @returns A nested list with information about the party.
#' This can be readily parsed by other functions.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' get_user_by_id()
#' }
#' }
#' @export
get_user_by_id <- function(user_id = 6, vb = options::opt("vb"), rq = NULL) {
  # Check parameters
  assertthat::assert_that(is.numeric(user_id))
  assertthat::assert_that(user_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  if (is.null(rq)) {
    if (vb) {
      message("\nNULL request object. Will generate default.")
      message("Not logged in. Only public information will be returned.")
    }
    rq <- databraryr::make_default_request()
  }
  
  endpoint <- GET_PARTY_BY_ID
  
  prq <- rq %>%
    httr2::req_url(sprintf(endpoint, user_id))
  
  if (vb)
    message("Querying API for 'user id'= ", user_id, ".")
  resp <- tryCatch(
    httr2::req_perform(prq),
    httr2_error = function(cnd) {
      if (vb)
        message("Error retrieving information for 'user_id'= ", user_id)
      NULL
    }
  )
  
  if (is.null(resp)) {
    message("Cannot access requested resource on Databrary. Exiting.")
    return(resp)
  } else {
    httr2::resp_body_json(resp)
  }
}
