#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Search For Institutions In Databrary.
#'
#' @description Perform a search across institutions registered with
#' Databrary.
#'
#' @param search_string Character string describing the institution search
#'   query.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing matching institutions ordered by relevance, or
#'   `NULL` when no matches exist for the query.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' search_institutions("state")
#' }
#' }
#' @export
search_institutions <- function(search_string,
                                vb = options::opt("vb"),
                                rq = NULL) {
  assertthat::assert_that(assertthat::is.string(search_string))
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  results <- collect_paginated_get(
    path = API_SEARCH_INSTITUTIONS,
    params = list(q = search_string),
    rq = rq,
    vb = vb
  )
  
  if (is.null(results) || length(results) == 0) {
    if (vb) {
      message("No institutions matched the search query '",
              search_string,
              "'.")
    }
    return(NULL)
  }
  
  purrr::map_dfr(results, function(entry) {
    tibble::tibble(
      institution_id = entry$id,
      institution_name = entry$name,
      institution_url = if (is.null(entry$url))
        NA_character_
      else
        entry$url,
      institution_has_avatar = if (is.null(entry$has_avatar))
        NA
      else
        entry$has_avatar,
      score = if (is.null(entry$score))
        NA_real_
      else
        entry$score
    )
  })
}
