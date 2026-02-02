#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Volumes Associated With A User
#'
#' @param user_id User identifier. Must be a positive integer. Default is 6.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is NULL.
#' 
#' @inheritParams options_params
#'
#' @return Tibble of volumes the user owns or collaborates on.
#' @export
list_user_volumes <- function(user_id = 6,
                              vb = options::opt("vb"),
                              rq = NULL) {
  assertthat::assert_that(is.numeric(user_id), length(user_id) == 1, user_id > 0)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  volumes <- collect_paginated_get(
    path = sprintf(API_USER_VOLUMES, user_id),
    rq = rq,
    vb = vb
  )

  if (is.null(volumes) || length(volumes) == 0) {
    if (vb) message("No volume data for user ", user_id)
    return(NULL)
  }

  user <- get_user_by_id(user_id, vb = vb, rq = rq)
  user_df <- tibble::as_tibble(user)

  purrr::map(volumes, function(entry) {
    tibble::tibble(
      vol_id = entry$id,
      vol_name = entry$title,
      vol_description = entry$description,
      vol_short_name = entry$short_name,
      vol_created_at = entry$created_at,
      vol_updated_at = entry$updated_at,
      vol_access_level = entry$access_level,
      vol_sharing_level = entry$sharing_level
    )
  }, .progress = TRUE) %>%
    purrr::list_rbind() %>%
    dplyr::mutate(user_id = user_df$id,
                  user_prename = user_df$prename,
                  user_sortname = user_df$sortname,
                  user_affiliation = user_df$affiliation) %>%
    dplyr::arrange(vol_id)
}

