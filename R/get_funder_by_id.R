#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Funder Information By ID
#'
#' @description Retrieve detailed information about a specific funder from
#' Databrary using its unique identifier.
#'
#' @param funder_id Numeric funder identifier. Must be a positive integer.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A list with the funder's metadata including id, name, and approval
#'   status, or `NULL` if the funder is not found or inaccessible.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Get details for a specific funder
#' get_funder_by_id(funder_id = 1)
#'
#' # Get funder information with verbose output
#' get_funder_by_id(funder_id = 1, vb = TRUE)
#' }
#' }
#' @export
get_funder_by_id <- function(funder_id = 1,
                             vb = options::opt("vb"),
                             rq = NULL) {
  assertthat::assert_that(is.numeric(funder_id))
  assertthat::assert_that(length(funder_id) == 1)
  assertthat::assert_that(funder_id > 0)
  assertthat::assert_that(funder_id == floor(funder_id), msg = "funder_id must be an integer")
  
  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  # Perform API call
  funder <- perform_api_get(
    path = sprintf(API_FUNDER_DETAIL, funder_id),
    rq = rq,
    vb = vb
  )
  
  if (is.null(funder)) {
    if (vb) {
      message("Funder ", funder_id, " not found or inaccessible.")
    }
    return(NULL)
  }
  
  # Return structured list
  list(
    funder_id = funder$id,
    funder_name = funder$name,
    funder_is_approved = funder$is_approved
  )
}
