#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Volume Record By ID
#'
#' @description Retrieve detailed information about a specific record
#' (participant data) from a Databrary volume using its unique identifier.
#' Records contain participant information including age, birthday, category,
#' and associated measures collected during sessions.
#'
#' @param vol_id Target volume number. Must be a positive integer. Default is 1.
#' @param record_id Numeric record identifier. Must be a positive integer. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A list aligned with the API record payload: `record_id`, `record_volume`,
#'   `record_volume_name`, `record_category_id`, `measures`, `birthday`, `age`,
#'   `default_sessions` (session ids/names where this record is a default),
#'   `record_source_kind` (linked-content provenance, e.g. `native`,
#'   `source_linked_file`). `record_volume` may differ from `vol_id` for linked
#'   records. Returns `NULL` if the record is not found or inaccessible.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Get details for a specific record
#' get_volume_record_by_id(vol_id = 1, record_id = 123)
#'
#' # Get record information with verbose output
#' get_volume_record_by_id(vol_id = 1, record_id = 123, vb = TRUE)
#' }
#' }
#' @export
get_volume_record_by_id <- function(
    vol_id = 1,
    record_id = 1,
    vb = options::opt("vb"),
    rq = NULL) {
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(vol_id >= 1)
  assertthat::assert_that(
    vol_id == floor(vol_id),
    msg = "vol_id must be an integer"
  )

  assertthat::assert_that(is.numeric(record_id))
  assertthat::assert_that(length(record_id) == 1)
  assertthat::assert_that(record_id > 0)
  assertthat::assert_that(
    record_id == floor(record_id),
    msg = "record_id must be an integer"
  )

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Perform API call
  record <- perform_api_get(
    path = sprintf(API_VOLUME_RECORD_DETAIL, vol_id, record_id),
    rq = rq,
    vb = vb
  )

  if (is.null(record)) {
    if (vb) {
      message(
        "Record ",
        record_id,
        " in volume ",
        vol_id,
        " not found or inaccessible."
      )
    }
    return(NULL)
  }

  record_as_client_list(record)
}
