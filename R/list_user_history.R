#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Account Activity For A Databrary User.
#'
#' @description Retrieve the OAuth and login activity history for a specific
#' user. Access is restricted to administrators and authorized investigators
#' with sufficient privileges.
#'
#' @param user_id Target user identifier. Must be a positive integer. Default is 22582.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing authentication and activity events for the
#'   selected user, or `NULL` when no entries are available.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_user_history(user_id = 22582)
#' }
#' }
#' @export
list_user_history <- function(user_id = 22582,
                              vb = options::opt("vb"),
                              rq = NULL) {
  assertthat::assert_that(is.numeric(user_id))
  assertthat::assert_that(length(user_id) == 1)
  assertthat::assert_that(user_id > 0)

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))

  history <- collect_paginated_get(
    path = sprintf(API_USERS_HISTORY, user_id),
    rq = rq,
    vb = vb
  )

  if (is.null(history) || length(history) == 0) {
    if (vb) {
      message("No activity history available for user ", user_id)
    }
    return(NULL)
  }

  purrr::map_dfr(history, function(entry) {
    tibble::tibble(
      user_id = user_id,
      history_id = entry$id,
      history_email = entry$email,
      history_ip_address = entry$ip_address,
      history_successful = entry$successful,
      history_type = entry$type,
      history_timestamp = entry$timestamp
    )
  })
}
