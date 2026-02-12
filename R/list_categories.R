#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Databrary Categories
#'
#' @description Retrieve all available categories from Databrary. Categories
#' define different types of data collection sessions and include nested
#' metrics that specify the data fields collected for each category.
#'
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing metadata for each category including id, name,
#'   description, and nested metrics, or `NULL` when no results are available.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # List all categories
#' list_categories()
#'
#' # List with verbose output
#' list_categories(vb = TRUE)
#' }
#' }
#' @export
list_categories <- function(vb = options::opt("vb"), rq = NULL) {
  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))

  # Perform API call
  categories <- perform_api_get(path = API_CATEGORIES, rq = rq, vb = vb)

  if (is.null(categories) || length(categories) == 0) {
    if (vb) {
      message("No categories available.")
    }
    return(NULL)
  }

  # Process categories into tibble
  purrr::map_dfr(categories, function(category) {
    # Process metrics if present
    metrics <- NULL
    if (!is.null(category$metrics) &&
          length(category$metrics) > 0) {
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

    tibble::tibble(
      category_id = category$id,
      category_name = category$name,
      category_description = if (is.null(category$description)) {
        NA_character_
      } else {
        category$description
      },
      metrics = list(metrics)
    )
  })
}
