#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Category Information By ID
#'
#' @description Retrieve detailed information about a specific category from
#' Databrary using its unique identifier. Categories include nested metrics
#' that define data collection fields.
#'
#' @param category_id Numeric category identifier. Must be a positive integer.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A list with the category's metadata including id, name, description,
#'   and nested metrics, or `NULL` if the category is not found or inaccessible.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Get details for a specific category
#' get_category_by_id(category_id = 1)
#'
#' # Get category information with verbose output
#' get_category_by_id(category_id = 1, vb = TRUE)
#' }
#' }
#' @export
get_category_by_id <- function(category_id = 1,
                               vb = options::opt("vb"),
                               rq = NULL) {
  # Validate category_id
  assertthat::assert_that(is.numeric(category_id))
  assertthat::assert_that(length(category_id) == 1)
  assertthat::assert_that(category_id > 0)
  assertthat::assert_that(category_id == floor(category_id), msg = "category_id must be an integer")
  
  # Validate vb
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Validate rq
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  # Perform API call
  category <- perform_api_get(
    path = sprintf(API_CATEGORY_DETAIL, category_id),
    rq = rq,
    vb = vb
  )
  
  if (is.null(category)) {
    if (vb) {
      message("Category ", category_id, " not found or inaccessible.")
    }
    return(NULL)
  }
  
  # Process metrics if present
  metrics <- NULL
  if (!is.null(category$metrics) && length(category$metrics) > 0) {
    metrics <- lapply(category$metrics, function(metric) {
      list(
        metric_id = metric$id,
        metric_name = metric$name,
        metric_type = metric$type,
        metric_release = metric$release,
        metric_options = metric$options,
        metric_assumed = metric$assumed,
        metric_description = metric$description,
        metric_required = metric$required
      )
    })
  }
  
  # Return structured list
  list(
    category_id = category$id,
    category_name = category$name,
    category_description = category$description,
    metrics = metrics
  )
}
