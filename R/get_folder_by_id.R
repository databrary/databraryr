#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Folder Metadata From a Databrary Volume.
#'
#' @param folder_id Folder identifier within the specified volume.
#' @param vol_id Volume identifier containing the folder.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @returns A list representing the folder metadata, or `NULL` when the folder
#'   cannot be accessed.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' get_folder_by_id() # Default folder in volume 1
#' }
#' }
#' @export
get_folder_by_id <- function(folder_id = 1,
                             vol_id = 1,
                             vb = options::opt("vb"),
                             rq = NULL) {
  assertthat::assert_that(length(folder_id) == 1)
  assertthat::assert_that(is.numeric(folder_id))
  assertthat::assert_that(folder_id >= 1)

  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  folder <- perform_api_get(
    path = sprintf(API_FOLDER_DETAIL, vol_id, folder_id),
    rq = rq,
    vb = vb
  )

  if (is.null(folder)) {
    if (vb) {
      message("Cannot access requested folder ", folder_id, " in volume ", vol_id)
    }
    return(NULL)
  }

  folder
}

