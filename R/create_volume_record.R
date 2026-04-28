#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Resolve the name/ID metric for a category in a volume
#'
#' @description Fetches the volume's enabled categories and metrics, then
#' finds the priority metric for the given category (name, id, or description).
#' Mirrors the frontend's \code{getPriorityMetric} logic.
#'
#' @param vol_id Target volume number.
#' @param category_id Category identifier.
#' @param vb Logical; if \code{TRUE}, print verbose messages.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @return The metric ID (integer) for the name field, or \code{NULL} if not found.
#' @noRd
get_name_metric_id <- function(vol_id,
                               category_id,
                               vb = options::opt("vb"),
                               rq = NULL) {
  volume <- perform_api_get(
    path = sprintf(API_VOLUME_DETAIL, vol_id),
    rq = rq,
    vb = vb
  )
  if (is.null(volume)) {
    return(NULL)
  }

  enabled_categories <- volume$enabled_categories
  enabled_metrics <- volume$enabled_metrics

  if (is.null(enabled_categories) || is.null(enabled_metrics)) {
    return(NULL)
  }

  # Find the category
  category <- NULL
  for (cat in enabled_categories) {
    if (identical(as.integer(cat$id), as.integer(category_id))) {
      category <- cat
      break
    }
  }
  if (is.null(category)) {
    return(NULL)
  }

  category_metrics <- category$metrics
  if (is.null(category_metrics) || length(category_metrics) == 0) {
    return(NULL)
  }

  # Get enabled metric IDs for this volume
  enabled_metric_ids <- vapply(
    enabled_metrics,
    function(m) as.integer(m$id),
    integer(1)
  )

  # Filter category metrics to those enabled for the volume
  category_metric_ids <- vapply(
    category_metrics,
    function(m) as.integer(m$id),
    integer(1)
  )
  available_metrics <- category_metrics[
    category_metric_ids %in% enabled_metric_ids
  ]
  if (length(available_metrics) == 0) {
    return(NULL)
  }

  # Priority: required first, then name, id, description (same as frontend)
  priority_names <- c("name", "id", "description")

  for (metric in available_metrics) {
    if (isTRUE(metric$required)) {
      return(as.integer(metric$id))
    }
  }
  for (pname in priority_names) {
    for (metric in available_metrics) {
      if (tolower(metric$name) == pname) {
        return(as.integer(metric$id))
      }
    }
  }

  # Fallback: first available metric
  as.integer(available_metrics[[1]]$id)
}

#' Create Record in Databrary Volume
#'
#' @description Create a new record in a Databrary volume. Records contain
#' metadata organized by category (e.g., participant, condition, task) with
#' measures (field values) stored per metric. The \code{name} is required and
#' resolved to the category's name/ID metric automatically. Use \code{measures}
#' to add additional metric values.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param category_id Numeric category identifier for the record type
#'   (e.g., participant, condition, task).
#' @param name Display name for the record (e.g., "P001", "Control group").
#'   Required; resolves to the category's name metric from volume configuration.
#' @param measures Optional named list mapping additional metric IDs (as strings)
#'   to values. Values can be strings (for text metrics), numbers (for numeric
#'   metrics), or lists with \code{year}, \code{month}, \code{day},
#'   optional \code{month} and \code{day} fields (for date metrics).
#' @param participant Optional list for participant records containing
#'   \code{birthday} (with \code{year}, \code{month}, \code{day} fields) or
#'   \code{age} (with \code{years}, \code{months}, \code{days} fields). Cannot
#'   provide both \code{birthday} and \code{age}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return Same shape as \code{\link{get_volume_record_by_id}}, or \code{NULL}
#'   if creation fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Create a task record with name only
#' create_volume_record(
#'   vol_id = 1,
#'   category_id = 6,
#'   name = "Control group"
#' )
#'
#' # Create a record with name and additional measures
#' create_volume_record(
#'   vol_id = 1,
#'   category_id = 6,
#'   name = "Task A",
#'   measures = list("30" = "Extra value")
#' )
#'
#' # Create a participant record with name and birthday
#' create_volume_record(
#'   vol_id = 1,
#'   category_id = 1,
#'   name = "P001",
#'   participant = list(
#'     birthday = list(year = 2020, month = 3, day = 15)
#'   )
#' )
#' }
#' }
#' @export
create_volume_record <- function(
  vol_id = 1,
  category_id,
  name,
  measures = list(),
  participant = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(category_id, "category_id")

  assertthat::assert_that(is.character(name))
  assertthat::assert_that(length(name) == 1)
  assertthat::assert_that(nzchar(trimws(name)), msg = "name must not be empty")

  assertthat::assert_that(is.list(measures))

  if (!is.null(participant)) {
    assertthat::assert_that(is.list(participant))
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Resolve name metric and build measures with name
  name_metric_id <- get_name_metric_id(
    vol_id = vol_id,
    category_id = category_id,
    vb = vb,
    rq = rq
  )

  if (is.null(name_metric_id)) {
    if (vb) {
      message(
        "Could not find name/ID metric for category ", category_id,
        " in volume ", vol_id
      )
    }
    return(NULL)
  }

  measures_with_name <- measures
  measures_with_name[[as.character(name_metric_id)]] <- name

  # Build request body
  body <- list(category_id = category_id, measures = measures_with_name)

  if (!is.null(participant)) {
    body$participant <- participant
  }

  # Perform API call
  record <- perform_api_post(
    path = sprintf(API_VOLUME_RECORDS, vol_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(record)) {
    if (vb) {
      message("Failed to create record in volume ", vol_id)
    }
    return(NULL)
  }

  record_as_client_list(record)
}
