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

  g <- get_party_by_id(party_id, vb, rq)
  
  resp <- NULL
  
  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    purrr::map(g$children, as.data.frame) |> 
      purrr::list_rbind() |>
      # TODO(ROG): Handle cases when party.orcid or other variables exist
      dplyr::select(party.id, 
                    party.sortname,
                    party.prename,
                    party.affiliation) |>
      dplyr::rename(affiliate_id = party.id,
                    affiliate_sortname = party.sortname,
                    affiliate_prename = party.prename,
                    affiliate_affiliation = party.affiliation) |>
      dplyr::mutate(party_id = party_id, 
                    party_sortname = g$sortname,
                    party_prename = g$prename,
                    party_affiliation = g$affiliation) |>
      dplyr::select(party_id,
                    party_sortname,
                    party_prename,
                    party_affiliation,
                    affiliate_id,
                    affiliate_sortname,
                    affiliate_prename,
                    affiliate_affiliation)
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
