#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Collaborators On A Databrary Volume.
#'
#' @description Retrieve collaboration metadata for a specified volume,
#' including sponsor details and access levels.
#'
#' @param vol_id Target volume number. Must be a positive integer. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble summarizing collaborator relationships on the volume, or
#'   `NULL` when no collaborators are associated with the volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_collaborators(vol_id = 1)
#' }
#' }
#' @export
list_volume_collaborators <- function(vol_id = 1,
                                      vb = options::opt("vb"),
                                      rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  validate_flag(vb, "vb")
  
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  collaborators <- perform_api_get(
    path = sprintf(API_VOLUME_COLLABORATORS, vol_id),
    rq = rq,
    vb = vb
  )
  
  if (is.null(collaborators) || length(collaborators) == 0) {
    if (vb) {
      message("No collaborators found for volume ", vol_id)
    }
    return(NULL)
  }
  
  purrr::map_dfr(collaborators, function(entry) {
    user <- entry$user
    sponsor <- entry$sponsor
    
    sponsor_id <- if (!is.null(sponsor))
      sponsor$id
    else
      NA_integer_
    sponsor_first <- if (!is.null(sponsor))
      sponsor$first_name
    else
      NA_character_
    sponsor_last <- if (!is.null(sponsor))
      sponsor$last_name
    else
      NA_character_
    sponsor_email <- if (!is.null(sponsor))
      sponsor$email
    else
      NA_character_
    
    tibble::tibble(
      collaborator_id = entry$id,
      volume_id = vol_id,
      collaborator_user_id = if (is.null(user))
        NA_integer_
      else
        user$id,
      collaborator_first_name = if (is.null(user))
        NA_character_
      else
        user$first_name,
      collaborator_last_name = if (is.null(user))
        NA_character_
      else
        user$last_name,
      collaborator_email = if (is.null(user))
        NA_character_
      else
        user$email,
      collaborator_is_authorized_investigator = if (is.null(user$is_authorized_investigator))
        NA
      else
        user$is_authorized_investigator,
      collaborator_has_avatar = if (is.null(user$has_avatar))
        NA
      else
        user$has_avatar,
      sponsor_user_id = sponsor_id,
      sponsor_first_name = sponsor_first,
      sponsor_last_name = sponsor_last,
      sponsor_email = sponsor_email,
      access_level = if (is.null(entry$access_level))
        NA_character_
      else
        entry$access_level,
      is_publicly_visible = if (is.null(entry$is_publicly_visible))
        NA
      else
        entry$is_publicly_visible,
      expiration_date = if (is.null(entry$expiration_date))
        NA_character_
      else
        entry$expiration_date
    )
  }, .progress = TRUE)
}
