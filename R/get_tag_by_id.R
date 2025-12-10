#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Tag Information By ID
#'
#' @description Retrieve detailed information about a specific tag from
#' Databrary using its unique identifier.
#'
#' @param tag_id Numeric tag identifier. Must be a positive integer.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A list with the tag's metadata including id and name,
#'   or `NULL` if the tag is not found or inaccessible.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Get details for a specific tag
#' get_tag_by_id(tag_id = 1)
#'
#' # Get tag information with verbose output
#' get_tag_by_id(tag_id = 1, vb = TRUE)
#' }
#' }
#' @export
get_tag_by_id <- function(tag_id = 1,
                          vb = options::opt("vb"),
                          rq = NULL) {
  # Validate tag_id
  assertthat::assert_that(is.numeric(tag_id))
  assertthat::assert_that(length(tag_id) == 1)
  assertthat::assert_that(tag_id > 0)
  assertthat::assert_that(tag_id == floor(tag_id),
                          msg = "tag_id must be an integer")

  # Validate vb
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  # Validate rq
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # Perform API call
  tag <- perform_api_get(
    path = sprintf(API_TAG_DETAIL, tag_id),
    rq = rq,
    vb = vb
  )

  if (is.null(tag)) {
    if (vb) {
      message("Tag ", tag_id, " not found or inaccessible.")
    }
    return(NULL)
  }

  # Return structured list
  list(
    tag_id = tag$id,
    tag_name = tag$name
  )
}
