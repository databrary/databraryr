#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Set Enabled Categories for a Volume
#'
#' @description Replace the full set of enabled categories for a Databrary
#' volume. This is a destructive replacement -- categories not in the provided
#' list will be disabled. Pass an empty vector to disable all categories.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param category_ids Integer vector of category IDs to enable.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} on success, \code{NULL} on failure.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Enable participant (1) and task (6) categories
#' set_volume_enabled_categories(vol_id = 1, category_ids = c(1, 6))
#'
#' # Disable all categories
#' set_volume_enabled_categories(vol_id = 1, category_ids = integer(0))
#' }
#' }
#' @export
set_volume_enabled_categories <- function(
  vol_id = 1,
  category_ids,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assertthat::assert_that(is.numeric(category_ids) || length(category_ids) == 0)
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  body <- as.integer(category_ids)

  result <- perform_api_post(
    path = sprintf(API_VOLUME_CATEGORIES, vol_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(result)) {
    if (vb) {
      message("Failed to update categories for volume ", vol_id)
    }
    return(NULL)
  }

  TRUE
}
