#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Databrary Users.
#'
#' @description Retrieve directory metadata for Databrary users. Results can be
#' filtered by name or restricted to specific account types using optional
#' parameters.
#'
#' @param search Optional character string used to filter results by name or
#'   email address.
#' @param include_suspended Optional logical value. When `TRUE`, suspended
#'   accounts are included in the response.
#' @param exclude_self Optional logical value. When `TRUE`, the authenticated
#'   user is omitted from the results.
#' @param is_authorized_investigator Optional logical value restricting the
#'   response to authorized investigators.
#' @param has_api_access Optional logical value restricting the response to
#'   accounts with API access enabled.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing directory metadata for each user, or `NULL` when
#'   no results are available for the supplied filters.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_users(search = "gilmore")
#' }
#' }
#' @export
list_users <- function(search = NULL,
                       include_suspended = NULL,
                       exclude_self = NULL,
                       is_authorized_investigator = NULL,
                       has_api_access = NULL,
                       vb = options::opt("vb"),
                       rq = NULL) {
  if (!is.null(search)) {
    assertthat::assert_that(assertthat::is.string(search))
  }

  validate_flag(include_suspended, "include_suspended", optional = TRUE)
  validate_flag(exclude_self, "exclude_self", optional = TRUE)
  validate_flag(is_authorized_investigator, "is_authorized_investigator", optional = TRUE)
  validate_flag(has_api_access, "has_api_access", optional = TRUE)

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  users <- collect_paginated_get(
    path = API_USERS,
    params = list(
      search = search,
      include_suspended = include_suspended,
      exclude_self = exclude_self,
      is_authorized_investigator = is_authorized_investigator,
      has_api_access = has_api_access
    ),
    rq = rq,
    vb = vb
  )

  if (is.null(users) || length(users) == 0) {
    if (vb) {
      message("No users matched the supplied filters.")
    }
    return(NULL)
  }

  purrr::map_dfr(users, function(user) {
    affiliation <- user$affiliation

    affiliation_id <- if (!is.null(affiliation)) affiliation$id else NA_integer_
    affiliation_name <- if (!is.null(affiliation)) affiliation$name else NA_character_

    suspended_by <- user$suspended_by
    suspended_by_id <- if (!is.null(suspended_by)) suspended_by$id else NA_integer_
    suspended_by_email <- if (!is.null(suspended_by)) suspended_by$email else NA_character_

    tibble::tibble(
      user_id = user$id,
      user_first_name = user$first_name,
      user_last_name = user$last_name,
      user_email = user$email,
      user_orcid = if (is.null(user$orcid)) NA_character_ else user$orcid,
      user_url = if (is.null(user$url)) NA_character_ else user$url,
      user_affiliation_id = affiliation_id,
      user_affiliation_name = affiliation_name,
      user_is_authorized_investigator = user$is_authorized_investigator,
      user_has_avatar = user$has_avatar,
      user_is_suspended = if (is.null(user$is_suspended)) NA else user$is_suspended,
      user_suspended_by_id = suspended_by_id,
      user_suspended_by_email = suspended_by_email,
      user_institution_sponsorships = list(user$institution_sponsorships),
      user_current_affiliates = list(user$current_affiliates),
      user_current_sponsors = list(user$current_sponsors)
    )
  })
}
