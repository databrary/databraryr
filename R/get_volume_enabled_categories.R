#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Enabled Categories for a Volume
#'
#' @description Retrieve the list of categories currently enabled for a
#' Databrary volume. Returns the \code{enabled_categories} field from the
#' volume detail endpoint.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list of category objects (each with \code{id}, \code{name},
#'   \code{metrics}, etc.), or \code{NULL} if the volume is not found.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' get_volume_enabled_categories(vol_id = 1)
#' }
#' }
#' @export
get_volume_enabled_categories <- function(
  vol_id = 1,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  volume <- perform_api_get(
    path = sprintf(API_VOLUME_DETAIL, vol_id),
    rq = rq,
    vb = vb
  )

  if (is.null(volume)) {
    if (vb) message("Volume ", vol_id, " not found or inaccessible.")
    return(NULL)
  }

  volume$enabled_categories
}
