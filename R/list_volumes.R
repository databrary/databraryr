#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Volumes Accessible Through The Databrary API.
#'
#' @description Returns summary metadata for volumes accessible to the
#' authenticated user. Results can be filtered by search term or ordering.
#'
#' @param search Optional character string used to filter volumes by title or
#'   description.
#' @param ordering Optional character string indicating the sort field accepted
#'   by the API (e.g., `"title"`, `"-title"`).
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @returns A tibble summarizing each accessible volume, or `NULL` when no
#'   volumes match the supplied filters.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volumes(search = "workshop")
#' }
#' }
#' @export
list_volumes <- function(search = NULL,
                         ordering = NULL,
                         vb = options::opt("vb"),
                         rq = NULL) {
  if (!is.null(search)) {
    assertthat::assert_that(assertthat::is.string(search))
  }
  if (!is.null(ordering)) {
    assertthat::assert_that(assertthat::is.string(ordering))
  }

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  volumes <- collect_paginated_get(
    path = API_VOLUMES,
    params = list(
      search = search,
      ordering = ordering
    ),
    rq = rq,
    vb = vb
  )

  if (is.null(volumes) || length(volumes) == 0) {
    if (vb) {
      message("No volumes matched the supplied filters.")
    }
    return(NULL)
  }

  purrr::map_dfr(volumes, function(volume) {
    owner_connection <- volume$owner_connection
    owner_user <- if (!is.null(owner_connection)) owner_connection$user else NULL
    owner_institution <- volume$owner_institution

    tibble::tibble(
      volume_id = volume$id,
      volume_title = volume$title,
      volume_short_name = if (is.null(volume$short_name)) NA_character_ else volume$short_name,
      volume_sharing_level = volume$sharing_level,
      volume_access_level = volume$access_level,
      volume_owner_connection_id = if (is.null(owner_connection)) NA_integer_ else owner_connection$id,
      volume_owner_role = if (is.null(owner_connection$role)) NA_character_ else owner_connection$role,
      volume_owner_expiration_date = if (is.null(owner_connection$expiration_date)) NA_character_ else owner_connection$expiration_date,
      volume_owner_user_id = if (is.null(owner_user)) NA_integer_ else owner_user$id,
      volume_owner_user_first_name = if (is.null(owner_user$first_name)) NA_character_ else owner_user$first_name,
      volume_owner_user_last_name = if (is.null(owner_user$last_name)) NA_character_ else owner_user$last_name,
      volume_owner_user_email = if (is.null(owner_user$email)) NA_character_ else owner_user$email,
      volume_owner_institution_id = if (is.null(owner_institution)) NA_integer_ else owner_institution$id,
      volume_owner_institution_name = if (is.null(owner_institution$name)) NA_character_ else owner_institution$name
    )
  })
}


