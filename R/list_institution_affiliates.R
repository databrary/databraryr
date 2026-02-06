#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List affiliates for an institution
#'
#' @param institution_id Institution identifier. Must be a positive integer. Default is 12.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @inheritParams options_params
#'
#' @return Tibble of affiliates with roles and expiration dates.
#' @export
list_institution_affiliates <- function(institution_id = 12,
                                        vb = options::opt("vb"),
                                        rq = NULL) {
  assertthat::assert_that(is.numeric(institution_id),
                          length(institution_id) == 1,
                          institution_id > 0)
  
  validate_flag(vb, "vb")
  
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  affiliates <- collect_paginated_get(
    path = sprintf(API_INSTITUTION_AFFILIATES, institution_id),
    rq = rq,
    vb = vb
  )
  
  if (is.null(affiliates) || length(affiliates) == 0) {
    if (vb)
      message("No affiliates for institution ", institution_id)
    return(NULL)
  }
  
  purrr::map_dfr(affiliates, function(entry) {
    user <- entry$user
    tibble::tibble(
      institution_id = institution_id,
      role = entry$role,
      access_level = entry$access_level,
      user_id = user$id,
      user_prename = user$first_name,
      user_sortname = user$last_name,
      user_affiliation = user$affiliation$name,
      user_affiliation_id = user$affiliation$id,
      expiration_date = entry$expiration_date
    )
  })
}
