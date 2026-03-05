#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List affiliates for a user
#'
#' @param user_id User identifier. Must be an integer. Default is 6.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#' 
#' @inheritParams options_params
#'
#' @return Tibble of affiliates for the user.
#' @export
list_user_affiliates <- function(user_id = 6,
                                 vb = options::opt("vb"),
                                 rq = NULL) {
  assertthat::assert_that(is.numeric(user_id), length(user_id) == 1, user_id > 0)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  validate_flag(vb, "vb")
  
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  affiliates <- collect_paginated_get(
    path = sprintf(API_USER_AFFILIATES, user_id),
    rq = rq,
    vb = vb
  )

  if (is.null(affiliates) || length(affiliates) == 0) {
    if (vb) message("No affiliates for user ", user_id)
    return(NULL)
  }

  purrr::map_dfr(affiliates, function(entry) {
    tibble::tibble(
      affiliate_user = entry$user,
      access_level = entry$access_level,
      role = entry$role,
      expiration_date = entry$expiration_date
    )
  })
}

