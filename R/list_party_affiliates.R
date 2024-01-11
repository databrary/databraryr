#' List Affiliates For A Party
#'
#' @param party_id Target party ID.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about a party's sponsors.
#' @examples
#' \donttest{
#' list_party_affiliates() # Default is Rick Gilmore (party 6)
#' }
#' @export
list_party_affiliates <- function(party_id = 6, 
                          vb = FALSE,
                          rq = NULL) {
  
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id > 0)
  assertthat::assert_that(is.logical(vb))
  
  party_info <- get_party_by_id(party_id, vb, rq)

  if (vb)
    message(paste0("Getting affiliates for party ", party_id, "."))
  # g <-
  #   databraryr::GET_db_contents(
  #     URL_components = paste0("/api/party/", party_id,
  #                             "?parents&children&access"),
  #     vb = vb
  #   )

  g <- get_party_by_id(party_id, vb, rq)
  
  resp <- NULL
  
  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    #p <- g$parents$party
    # 
    # if (!is.null(p)) {
    #   p
    # } else {
    #   NULL
    # }
    purrr::map(g$children, as.data.frame) |> purrr::list_rbind()
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
