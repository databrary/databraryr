#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Request a Signed ZIP Download for a Folder.
#'
#' @description
#' Folder-level ZIP archives are prepared asynchronously by the Django API.
#' Calling `download_folder_zip()` queues the job and returns a processing task
#' descriptor. When the archive is ready, Databrary emails a signed download
#' link to the authenticated user.
#'
#' @param vol_id Volume identifier for the folder.
#' @param folder_id Folder identifier scoped within the specified volume.
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
#' download_folder_zip(vol_id = 1, folder_id = 1)
#' }
#' }
#'
#' @export
download_folder_zip <- function(vol_id = 1,
                                folder_id = 1,
                                vb = options::opt("vb"),
                                rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)

  assertthat::assert_that(length(folder_id) == 1)
  assertthat::assert_that(is.numeric(folder_id))
  assertthat::assert_that(folder_id >= 1)

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  assertthat::assert_that(is.null(rq) || ("httr2_request" %in% class(rq)))

  path <- sprintf(API_FOLDER_DOWNLOAD_LINK, vol_id, folder_id)
  request_processing_task(path = path, rq = rq, vb = vb)
}



