#' @eval options::as_params()
#' @name options_params
#' 
NULL

#' Request a Signed ZIP Download for a Volume.
#'
#' @description
#' Volume-level ZIP archives are prepared asynchronously by the Django API.
#' Calling `download_volume_zip()` queues the job and returns a processing task
#' descriptor. When the archive is ready, Databrary emails a signed download
#' link to the authenticated user.
#'
#' @param vol_id An integer. Volume identifier. Default is 31.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is `NULL`, in which case a
#'   default authenticated request is generated.
#'
#' @returns A list describing the processing task (`status`, `message`,
#'   `task_id`) or `NULL` when the request fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' download_volume_zip(vol_id = 31)
#' }
#' }
#'
#' @export
download_volume_zip <- function(vol_id = 31,
                                vb = options::opt("vb"),
                                rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  assertthat::assert_that(is.null(rq) || ("httr2_request" %in% class(rq)))

  path <- sprintf(API_VOLUME_DOWNLOAD_LINK, vol_id)
  request_processing_task(path = path, rq = rq, vb = vb)
}
