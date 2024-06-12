#' Download Databrary Constants From API.
#' 
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object. Defaults to NULL.
#'
#' @returns A data frame with the constants.
#' 
#' @examples
#' \donttest{
#' assign_constants()
#' }
#' @export
assign_constants <- function(vb = FALSE, rq = NULL) {
  # Check parameter
  assertthat::assert_that(is.logical(vb))
  
  if (is.null(rq))
    rq <- databraryr::make_default_request()
  arq <- rq %>%
    httr2::req_url(GET_CONSTANTS)
  
  resp <- tryCatch(
    httr2::req_perform(arq),
    httr2_error = function(cnd) {
      if (vb) message("Error loading Databrary constants.")
      NULL
    }
  )
  
  if (is.null(resp)) {
    if (vb) message("Exiting")
    resp
  } else {
    httr2::resp_body_json(resp)
  }
}
