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
  # Validate vol_id
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  assertthat::assert_that(
    vol_id == floor(vol_id),
    msg = "vol_id must be an integer"
  )

  # Validate record_id
  assertthat::assert_that(is.numeric(record_id))
  assertthat::assert_that(length(record_id) == 1)
  assertthat::assert_that(record_id > 0)
  assertthat::assert_that(
    record_id == floor(record_id),
    msg = "record_id must be an integer"
  )

  # Validate metric_id
  assertthat::assert_that(is.numeric(metric_id))
  assertthat::assert_that(length(metric_id) == 1)
  assertthat::assert_that(metric_id > 0)
  assertthat::assert_that(
    metric_id == floor(metric_id),
    msg = "metric_id must be an integer"
  )

  # Validate vb
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  # Validate rq
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
