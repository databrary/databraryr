#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Search For Users In Databrary.
#'
#' @description Perform a directory search across Databrary users by name or
#' email address.
#'
#' @param search_string Character string describing the search query.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing user matches ordered by relevance, or `NULL`
#'   when no matches exist for the query.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' search_users("gilmore")
#' }
#' }
#' @export
search_users <- function(search_string,
                         vb = options::opt("vb"),
                         rq = NULL) {
  assertthat::assert_that(assertthat::is.string(search_string))
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  results <- collect_paginated_get(
    path = API_SEARCH_USERS,
    params = list(q = search_string),
    rq = rq,
    vb = vb
  )

  if (is.null(results) || length(results) == 0) {
    if (vb) {
      message("No users matched the search query '", search_string, "'.")
    }
    return(NULL)
  }

  purrr::map_dfr(results, function(entry) {
    tibble::tibble(
      user_id = entry$id,
      user_first_name = entry$first_name,
      user_last_name = entry$last_name,
      user_full_name = entry$full_name,
      user_email = entry$email,
      user_orcid = if (is.null(entry$orcid)) NA_character_ else entry$orcid,
      user_url = if (is.null(entry$url)) NA_character_ else entry$url,
      user_is_authorized = if (is.null(entry$is_authorized)) NA else entry$is_authorized,
      user_has_avatar = if (is.null(entry$has_avatar)) NA else entry$has_avatar,
      score = if (is.null(entry$score)) NA_real_ else entry$score
    )
  })
}


