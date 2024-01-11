#' List Sponsors For A Party
#'
#' @param party_id Target party ID.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2`-style request object. If NULL, then a new request will
#' be generated using `make_default_request()`.
#' @returns A data frame with information about a party's sponsors.
#' @examples
#' \donttest{
#' \dontrun{
#' list_party_sponsors() # Default is Rick Gilmore (party 6)
#' }
#' }
#' @export
list_party_sponsors <- function(party_id = 6, 
                          vb = FALSE,
                          rq = NULL) {
  
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id > 0)
  assertthat::assert_that(is.logical(vb))
  
  party_info <- get_party_by_id(party_id, vb, rq)

  if (vb)
    message(paste0("Getting sponsors for party ", party_id, "."))

  g <- get_party_by_id(party_id, vb, rq)

  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    purrr::map(g$parents, as.data.frame) |> 
      purrr::list_rbind() |>
      dplyr::rename(sponsor_id = party.id,
                    sponsor_sortname = party.sortname,
                    sponsor_affiliation = party.affiliation,
                    sponsor_institution = party.institution,
                    sponsor_url = party.url) |>
      dplyr::mutate(party_id = party_id, 
                    party_sortname = g$sortname,
                    party_prename = g$prename,
                    party_affiliation = g$affiliation,
                    party_url = g$url)
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
