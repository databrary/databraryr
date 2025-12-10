#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Institutions
#'
#' @description Retrieve a list of all institutions registered with Databrary.
#' Optionally filter by search string.
#'
#' @param search_string Optional character string to filter institutions. If
#'   `NULL` (the default), returns all institutions.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing institutions with their metadata including id,
#'   name, url, date_signed, source, created_at, updated_at, has_avatar,
#'   has_administrators, latitude, longitude, and manual_coordinates, or `NULL`
#'   if no institutions are found.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # List all institutions
#' list_institutions()
#'
#' # List institutions filtered by search string
#' list_institutions(search_string = "university")
#'
#' # With verbose output
#' list_institutions(vb = TRUE)
#' }
#' }
#' @export
list_institutions <- function(search_string = NULL,
                               vb = options::opt("vb"),
                               rq = NULL) {
  # Validate search_string
  if (!is.null(search_string)) {
    assertthat::assert_that(assertthat::is.string(search_string))
  }

  # Validate vb
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  # Validate rq
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Build params list
  params <- list()
  if (!is.null(search_string)) {
    params$search <- search_string
  }

  # Perform API call with pagination
  results <- collect_paginated_get(
    path = API_INSTITUTIONS_LIST,
    params = params,
    rq = rq,
    vb = vb
  )

  if (is.null(results) || length(results) == 0) {
    if (vb) {
      if (is.null(search_string)) {
        message("No institutions found.")
      } else {
        message("No institutions found matching '", search_string, "'.")
      }
    }
    return(NULL)
  }

  # Process results into tibble
  purrr::map_dfr(results, function(entry) {
    tibble::tibble(
      institution_id = entry$id,
      institution_name = entry$name,
      institution_url = if (is.null(entry$url)) NA_character_ else entry$url,
      institution_date_signed = if (is.null(entry$date_signed)) NA_character_ else as.character(entry$date_signed),
      institution_source = if (is.null(entry$source)) NA_character_ else entry$source,
      institution_created_at = if (is.null(entry$created_at)) NA_character_ else as.character(entry$created_at),
      institution_updated_at = if (is.null(entry$updated_at)) NA_character_ else as.character(entry$updated_at),
      institution_has_avatar = if (is.null(entry$has_avatar)) NA else entry$has_avatar,
      institution_has_administrators = if (is.null(entry$has_administrators)) NA else entry$has_administrators,
      institution_latitude = if (is.null(entry$latitude)) NA_real_ else as.numeric(entry$latitude),
      institution_longitude = if (is.null(entry$longitude)) NA_real_ else as.numeric(entry$longitude),
      institution_manual_coordinates = if (is.null(entry$manual_coordinates)) NA else entry$manual_coordinates
    )
  })
}