#' Download Information About a Party on Databrary as JSON
#'
#' @param party_id An integer. The party number to retrieve information about.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @param query_param A string. The API query parameter. Defaults to 
#' "?parents&children" which returns profile information about the party and
#' information about their institutional or individual sponsors and the 
#' affiliates the party sponsors.
#' @param rq An `httr2`-style request object. If NULL, then a new request will
#' be generated using `make_default_request()`.
#' @returns A JSON blob with information about the party.
#' @examples
#' \donttest{
#' \dontrun{
#' get_party_by_id()
#' 
#' # Return information about volumes the party has access to.
#' get_party_by_id(query_param = "?parents&children&access")
#' }
#' }
#' @export
get_party_by_id <- function(party_id = 6,
                           vb = FALSE,
                           query_param = "?parents&children&access",
                           rq = NULL) {
  
  # Check parameters
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id >= 1)
  assertthat::assert_that(is.logical(vb))
  assertthat::is.string(query_param)
  
  if (is.null(rq)) {
    if (vb) message("No request object supplied. Using default.")
    rq <- make_default_request()
  }
  
  prq <- rq |>
    httr2::req_url(paste0(sprintf(GET_PARTY_BY_ID, party_id), query_param))
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
