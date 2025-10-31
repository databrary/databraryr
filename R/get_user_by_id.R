#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get public profile information for a Databrary user
#'
#' @param user_id User identifier.
#' @inheritParams options_params
#'
#' @return A list with the user's public metadata.
#' @export
get_user_by_id <- function(user_id = 6,
                           vb = options::opt("vb"),
                           rq = NULL) {
  assertthat::assert_that(is.numeric(user_id), length(user_id) == 1, user_id > 0)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  user <- perform_api_get(
    path = sprintf(API_USER_DETAIL, user_id),
    rq = rq,
    vb = vb
  )

  if (is.null(user)) {
    if (vb) message("User ", user_id, " not found or inaccessible.")
    return(NULL)
  }

  affiliation <- user$affiliation
  institution_name <- affiliation$name
  institution_id <- affiliation$id

  tibble::tibble(
    id = user$id,
    prename = user$first_name,
    sortname = user$last_name,
    email = user$email,
    affiliation = institution_name,
    affiliation_id = institution_id,
    is_authorized_investigator = user$is_authorized_investigator,
    has_avatar = user$has_avatar
  ) %>%
    as.list()
}

