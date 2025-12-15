#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List sponsorships for a user
#'
#' @param user_id User identifier.
#' @inheritParams options_params
#'
#' @return Tibble of sponsors for the user.
#' @export
list_user_sponsors <- function(user_id = 6,
                                vb = options::opt("vb"),
                                rq = NULL) {
  assertthat::assert_that(is.numeric(user_id), length(user_id) == 1, user_id > 0)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  sponsorships <- collect_paginated_get(
    path = sprintf(API_USER_SPONSORSHIPS, user_id),
    rq = rq,
    vb = vb
  )

  if (is.null(sponsorships) || length(sponsorships) == 0) {
    if (vb) message("No sponsorships for user ", user_id)
    return(NULL)
  }

  user <- get_user_by_id(user_id, vb = vb, rq = rq)

  purrr::map_dfr(sponsorships, function(entry) {
    sponsor <- entry$user
    tibble::tibble(
      user_id = user$id,
      user_prename = user$prename,
      user_sortname = user$sortname,
      user_affiliation = user$affiliation,
      sponsor_id = sponsor$id,
      sponsor_prename = sponsor$first_name,
      sponsor_sortname = sponsor$last_name,
      sponsor_affiliation = sponsor$affiliation$name,
      sponsor_affiliation_id = sponsor$affiliation$id,
      access_level = entry$access_level,
      role = entry$role,
      expiration_date = entry$expiration_date
    )
  })
}

