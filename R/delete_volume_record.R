#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Delete Record from Databrary Volume
#'
#' @description Delete (soft-delete) a record from a Databrary volume. The
#' record and its measures are marked as deleted but not permanently removed
#' from the database.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the record was successfully deleted, \code{FALSE}
#'   otherwise.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Delete a record
#' delete_volume_record(vol_id = 1, record_id = 123)
#'
#' # Delete with verbose output
#' delete_volume_record(vol_id = 1, record_id = 123, vb = TRUE)
#' }
#' }
#' @export
delete_volume_record <- function(
  vol_id = 1,
  record_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(record_id, "record_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Perform API call
  success <- perform_api_delete(
    path = sprintf(API_VOLUME_RECORD_DETAIL, vol_id, record_id),
    rq = rq,
    vb = vb
  )

  if (!success) {
    if (vb) {
      message(
        "Failed to delete record ",
        record_id,
        " from volume ",
        vol_id
      )
    }
  }

  success
}
