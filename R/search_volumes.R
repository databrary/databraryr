#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Search For Volumes In Databrary.
#'
#' @description Search across Databrary volumes using the Django search
#' endpoint.
#'
#' @param search_string Character string describing the volume search query.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing matching volumes ordered by relevance, or `NULL`
#'   when no matches exist for the query.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' search_volumes("workshop")
#' }
#' }
#' @export
search_volumes <- function(search_string,
                           vb = options::opt("vb"),
                           rq = NULL) {
  assertthat::assert_that(assertthat::is.string(search_string))
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  results <- collect_paginated_get(
    path = API_SEARCH_VOLUMES,
    params = list(q = search_string),
    rq = rq,
    vb = vb
  )

  if (is.null(results) || length(results) == 0) {
    if (vb) {
      message("No volumes matched the search query '", search_string, "'.")
    }
    return(NULL)
  }

  purrr::map_dfr(results, function(entry) {
    owner <- entry$owner

    owner_user_id <- NA_integer_
    owner_full_name <- NA_character_
    owner_institution_id <- NA_integer_
    owner_institution_name <- NA_character_

    if (!is.null(owner)) {
      if (!is.null(owner$user_id)) {
        owner_user_id <- owner$user_id
      } else {
        owner_user_id <- NA_integer_
      }
      owner_full_name <- if (is.null(owner$full_name)) NA_character_ else owner$full_name
      owner_institution_id <- if (is.null(owner$institution_id)) NA_integer_ else owner$institution_id
      owner_institution_name <- if (is.null(owner$institution_name)) NA_character_ else owner$institution_name
    }

    tibble::tibble(
      volume_id = entry$id,
      volume_title = entry$title,
      volume_description = if (is.null(entry$description)) NA_character_ else entry$description,
      volume_sharing_level = entry$sharing_level,
      owner_user_id = owner_user_id,
      owner_full_name = owner_full_name,
      owner_institution_id = owner_institution_id,
      owner_institution_name = owner_institution_name,
      tags = list(entry$tags),
      score = if (is.null(entry$score)) NA_real_ else entry$score
    )
  })
}


