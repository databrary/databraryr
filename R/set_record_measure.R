#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Set Measure for a Record
#'
#' @description Create or update a single measure for a record. This performs
#' an upsert operation - if the measure exists for this metric, it is updated;
#' otherwise, a new measure is created.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param metric_id Numeric metric identifier. Must be a positive integer.
#' @param value The measure value. Can be a string (for text metrics), a number
#'   (for numeric metrics), or a list with \code{year}, \code{month}, \code{day},
#'   \code{is_estimated} fields (for date metrics).
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return The measure data on success, or \code{NULL} if the operation fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Set a text measure
#' set_record_measure(
#'   vol_id = 1,
#'   record_id = 123,
#'   metric_id = 2,
#'   value = "Female"
#' )
#'
#' # Set a numeric measure
#' set_record_measure(
#'   vol_id = 1,
#'   record_id = 123,
#'   metric_id = 5,
#'   value = 24.5
#' )
#'
#' # Set a date measure
#' set_record_measure(
#'   vol_id = 1,
#'   record_id = 123,
#'   metric_id = 4,
#'   value = list(year = 2020, month = 3, day = 15, is_estimated = FALSE)
#' )
#' }
#' }
#' @export
set_record_measure <- function(
  vol_id = 1,
  record_id,
  metric_id,
  value,
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

  # Build request body
  # For date measures, value is already a list with year/month/day
  # For text/numeric measures, wrap in list(value = ...)
  body <- if (is.list(value) && !is.null(names(value))) {
    # Assume it's a date measure with named fields
    value
  } else {
    list(value = value)
  }

  # Perform API call
  measure <- perform_api_post(
    path = sprintf(API_RECORD_MEASURES, vol_id, record_id, metric_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(measure)) {
    if (vb) {
      message(
        "Failed to set measure for metric ",
        metric_id,
        " on record ",
        record_id,
        " in volume ",
        vol_id
      )
    }
    return(NULL)
  }

  measure
}
