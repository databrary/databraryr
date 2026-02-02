#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Volume Collaborator By ID
#'
#' @description Retrieve detailed information about a specific collaborator
#' on a Databrary volume using their unique collaborator identifier. Returns
#' collaborator details including user information, sponsor details, access
#' level, and visibility settings.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param collaborator_id Numeric collaborator identifier. Must be a positive integer.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A list with the collaborator's metadata including id, volume, user
#'   details, sponsor information (if applicable), access level, visibility
#'   settings, and expiration date, or `NULL` if the collaborator is not found
#'   or inaccessible.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Get details for a specific collaborator
#' get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 5)
#'
#' # Get collaborator information with verbose output
#' get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 5, vb = TRUE)
#' }
#' }
#' @export
get_volume_collaborator_by_id <- function(vol_id = 1,
                                          collaborator_id = 1,
                                          vb = options::opt("vb"),
                                          rq = NULL) {
  # Validate vol_id
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(vol_id >= 1)
  assertthat::assert_that(vol_id == floor(vol_id), msg = "vol_id must be an integer")
  
  # Validate collaborator_id
  assertthat::assert_that(is.numeric(collaborator_id))
  assertthat::assert_that(length(collaborator_id) == 1)
  assertthat::assert_that(collaborator_id > 0)
  assertthat::assert_that(collaborator_id == floor(collaborator_id), msg = "collaborator_id must be an integer")
  
  # Validate vb
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Validate rq
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  # Perform API call
  collaborator <- perform_api_get(
    path = sprintf(API_VOLUME_COLLABORATOR_DETAIL, vol_id, collaborator_id),
    rq = rq,
    vb = vb
  )
  
  if (is.null(collaborator)) {
    if (vb) {
      message(
        "Collaborator ",
        collaborator_id,
        " in volume ",
        vol_id,
        " not found or inaccessible."
      )
    }
    return(NULL)
  }
  
  # Process user information
  user <- NULL
  if (!is.null(collaborator$user)) {
    user <- list(
      user_id = collaborator$user$id,
      first_name = collaborator$user$first_name,
      last_name = collaborator$user$last_name,
      email = collaborator$user$email,
      is_authorized_investigator = collaborator$user$is_authorized_investigator,
      has_avatar = collaborator$user$has_avatar
    )
  }
  
  # Process sponsor information
  sponsor <- NULL
  if (!is.null(collaborator$sponsor)) {
    sponsor <- list(
      sponsor_id = collaborator$sponsor$id,
      first_name = collaborator$sponsor$first_name,
      last_name = collaborator$sponsor$last_name,
      email = collaborator$sponsor$email
    )
  }
  
  # Process sponsorship information
  sponsorship <- NULL
  if (!is.null(collaborator$sponsorship)) {
    sponsorship <- list(
      sponsorship_id = collaborator$sponsorship$id,
      sponsor_id = collaborator$sponsorship$sponsor,
      sponsored_user_id = collaborator$sponsorship$sponsored_user,
      status = collaborator$sponsorship$status
    )
  }
  
  # Process sponsored_users if present
  sponsored_users <- NULL
  if (!is.null(collaborator$sponsored_users) &&
      length(collaborator$sponsored_users) > 0) {
    sponsored_users <- lapply(collaborator$sponsored_users, function(u) {
      list(
        user_id = u$id,
        first_name = u$first_name,
        last_name = u$last_name,
        email = u$email
      )
    })
  }
  
  # Return structured list
  list(
    collaborator_id = collaborator$id,
    volume = collaborator$volume,
    user = user,
    sponsor = sponsor,
    sponsorship = sponsorship,
    is_publicly_visible = collaborator$is_publicly_visible,
    access_level = collaborator$access_level,
    expiration_date = collaborator$expiration_date,
    sponsored_users = sponsored_users
  )
}
