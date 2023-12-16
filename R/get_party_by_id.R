#' Download Information About a Party on Databrary.
#'
#' @param party_id An integer. The party number to retrieve information about.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @rq An `httr2`-style request object.
#' @returns A data frame with information about the party.
#' @examples
#' \donttest{
#' download_party()
#' #' }
#' @export
get_party_by_id <- function(party_id = 6,
                           vb = FALSE,
                           rq = NULL) {
  
  # Check parameters
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id >= 1)
  
  if (is.null(rq)) {
    rq <- make_default_request()
  }
  
  prq <- rq |>
    httr2::req_url(sprintf(GET_PARTY_BY_ID, party_id))
  resp <- tryCatch(
    httr2::req_perform(prq),
    httr2_error = function(cnd)
      NULL
  )
  if (!is.null(resp)) {
    httr2::resp_body_json(resp)
  } else {
    resp
  }
}
