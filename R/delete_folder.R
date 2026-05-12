#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Delete Folder from Databrary Volume
#'
#' @description Delete (soft-delete) a folder from a Databrary volume. The
#' folder and its associated metadata are marked as deleted but not
#' permanently removed from the database.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param folder_id Numeric folder identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the folder was successfully deleted, \code{FALSE}
#'   otherwise.
#'
#' @seealso \code{\link{create_folder}}, \code{\link{update_folder}},
#'   \code{\link{patch_folder}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' delete_folder(vol_id = 1, folder_id = 42)
#' }
#' }
#' @export
delete_folder <- function(
  vol_id = 1,
  folder_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(folder_id, "folder_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  success <- perform_api_delete(
    path = sprintf(API_FOLDER_DETAIL, vol_id, folder_id),
    rq = rq,
    vb = vb
  )

  if (!success) {
    if (vb) {
      message(
        "Failed to delete folder ", folder_id, " from volume ", vol_id
      )
    }
  }

  success
}
