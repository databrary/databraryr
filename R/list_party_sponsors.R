#' List Sponsors For A Party
#'
#' @param party_id Target party ID.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2`-style request object. If NULL, then a new request will
#' be generated using `make_default_request()`.
#'
#' @returns A data frame with information about a party's sponsors.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_party_sponsors() # Default is Rick Gilmore (party 6)
#' }
#' }
#'
#' @export
list_party_sponsors <- function(party_id = 6,
                                vb = FALSE,
                                rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id > 0)
  
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  party_info <- get_party_by_id(party_id, vb, rq)
  
  if (vb)
    message(paste0("Getting sponsors for party ", party_id, "."))
  
  g <- get_party_by_id(party_id, vb, rq)
  
  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    
    party.id <- NULL
    party.sortname <- NULL
    party.affiliation <- NULL
    party_sortname <- NULL
    party_prename <- NULL
    party_affiliation <- NULL
    party_url <- NULL
    sponsor_id <- NULL
    sponsor_shortname <- NULL
    sponsor_affiliation <- NULL
    
    purrr::map(g$parents, as.data.frame) |>
      purrr::list_rbind() |>
      # TODO(ROG): Handle cases when party.prename, expires, other variables exist
      dplyr::select(party.id,
                    party.sortname,
                    party.affiliation) |>
      dplyr::rename(
        sponsor_id = party.id,
        sponsor_sortname = party.sortname,
        sponsor_affiliation = party.affiliation
      ) |>
      dplyr::mutate(
        party_id = party_id,
        party_sortname = g$sortname,
        party_prename = g$prename,
        party_affiliation = g$affiliation,
        party_url = g$url
      ) |>
      dplyr::select(
        party_id,
        party_sortname,
        party_prename,
        party_affiliation,
        party_url,
        sponsor_id,
        sponsor_sortname,
        sponsor_affiliation
      )
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
