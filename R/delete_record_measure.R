#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Delete Measure from a Record
#'
#' @description Delete a single measure from a record. Note that the backend
#' will reject attempts to delete measures for required metrics.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param metric_id Numeric metric identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the measure was successfully deleted, \code{FALSE}
#'   otherwise. Deletion will fail for required metrics.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Delete a measure
#' delete_record_measure(
#'   vol_id = 1,
#'   record_id = 123,
#'   metric_id = 5
#' )
#'
#' # Delete with verbose output
#' delete_record_measure(
#'   vol_id = 1,
#'   record_id = 123,
#'   metric_id = 5,
#'   vb = TRUE
#' )
#' }
#' }
#' @export
delete_record_measure <- function(
  vol_id = 1,
  record_id,
  metric_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(record_id, "record_id")
  assert_positive_integer(metric_id, "metric_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Perform API call
  success <- perform_api_delete(
    path = sprintf(API_RECORD_MEASURES, vol_id, record_id, metric_id),
    rq = rq,
    vb = vb
  )

  if (!success) {
    if (vb) {
      message(
        "Failed to delete measure for metric ",
        metric_id,
        " on record ",
        record_id,
        " in volume ",
        vol_id
      )
    }
  }

  success
}
