#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Update Record in Databrary Volume
#'
#' @description Update an existing record in a Databrary volume. This performs
#' a partial update (PATCH), modifying only the fields provided.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param record_id Numeric record identifier. Must be a positive integer.
#' @param measures Optional named list mapping metric IDs (as strings) to values.
#'   Values can be strings (for text metrics), numbers (for numeric metrics),
#'   or lists with \code{year}, \code{month}, \code{day}, \code{is_estimated}
#'   fields (for date metrics). If provided, replaces existing measures.
#' @param participant Optional list for participant records containing
#'   \code{birthday} (with \code{year}, \code{month}, \code{day} fields) or
#'   \code{age} (with \code{years}, \code{months}, \code{days} fields).
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A list with the updated record's metadata including id, volume,
#'   category_id, measures, birthday (if participant), and age (if participant),
#'   or \code{NULL} if update fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Update measures for a record
#' update_volume_record(
#'   vol_id = 1,
#'   record_id = 123,
#'   measures = list("2" = "Male", "5" = 25.5)
#' )
#'
#' # Update participant birthday
#' update_volume_record(
#'   vol_id = 1,
#'   record_id = 123,
#'   participant = list(
#'     birthday = list(year = 2021, month = 5, day = 10)
#'   )
#' )
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

  # Process age if present
  age <- NULL
  if (!is.null(record$age)) {
    age <- list(
      years = record$age$years,
      months = record$age$months,
      days = record$age$days,
      total_days = record$age$total_days,
      formatted_value = record$age$formatted_value,
      is_estimated = record$age$is_estimated,
      is_blurred = record$age$is_blurred
    )
  }

  # Return structured list
  list(
    record_id = record$id,
    record_volume = record$volume,
    record_category_id = record$category_id,
    measures = record$measures,
    birthday = record$birthday,
    age = age
  )
}
