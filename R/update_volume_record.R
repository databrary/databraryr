#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Update Record in Databrary Volume
#'
#' @description Sends a PATCH request to update a record. The HTTP method is
#' partial in principle, but the API validates \code{measures} as a complete
#' snapshot of \strong{required} metrics for the record's category: every
#' required metric must appear in \code{measures}, or the server returns
#' \code{400} (\verb{Missing required measures...}). The server then replaces
#' stored values for each metric key you send (it does not merge your list with
#' existing measures before that check). To change a single metric without
#' assembling the full required set, use \code{\link{set_record_measure}}.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param measures Optional named list mapping metric IDs (as strings) to values.
#'   Values can be strings (for text metrics), numbers (for numeric metrics),
#'   or lists with \code{year} and optional \code{month}, \code{day}
#'   fields (for date metrics). When supplied, must include all required metrics
#'   for the record's category (use \code{\link{get_volume_enabled_categories}}
#'   or \code{\link{get_volume_record_by_id}} to discover ids and current values).
#' @param participant Optional list for participant records containing
#'   \code{birthday} (with \code{year}, \code{month}, \code{day} fields) or
#'   \code{age} (with \code{years}, \code{months}, \code{days} fields). Sending
#'   only \code{participant} without a complete \code{measures} map may still
#'   fail validation for categories with required metrics; include required
#'   measures or use \code{\link{set_record_measure}} for targeted edits.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @seealso \code{\link{set_record_measure}}, \code{\link{get_volume_record_by_id}}
#'
#' @return A list with the updated record's metadata (same shape as
#'   \code{\link{get_volume_record_by_id}}), or \code{NULL} if update fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Prefer for a single metric (no need to list all required measures)
#' set_record_measure(vol_id = 1, record_id = 123, metric_id = 2, value = "Male")
#'
#' # PATCH with measures: merge current values with changes so required metrics stay present
#' rec <- get_volume_record_by_id(vol_id = 1, record_id = 123)
#' new_measures <- utils::modifyList(rec$measures, list("2" = "Updated label"))
#' update_volume_record(vol_id = 1, record_id = 123, measures = new_measures)
#' }
#' }
#' @export
update_volume_record <- function(
  vol_id = 1,
  record_id,
  measures = NULL,
  participant = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(record_id, "record_id")

  if (!is.null(measures)) {
    assertthat::assert_that(is.list(measures))
  }

  if (!is.null(participant)) {
    assertthat::assert_that(is.list(participant))
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  body <- list()

  if (!is.null(measures)) {
    body$measures <- measures
  }

  if (!is.null(participant)) {
    body$participant <- participant
  }

  if (length(body) == 0) {
    if (vb) {
      message("No fields provided to update for record ", record_id)
    }
    return(NULL)
  }

  record <- perform_api_patch(
    path = sprintf(API_VOLUME_RECORD_DETAIL, vol_id, record_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(record)) {
    if (vb) {
      message("Failed to update record ", record_id, " in volume ", vol_id)
    }
    return(NULL)
  }

  record_as_client_list(record)
}
