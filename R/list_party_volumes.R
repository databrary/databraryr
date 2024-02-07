#' List Volumes For A Party
#'
#' @param party_id Target party ID.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2`-style request object. If NULL, then a new request will
#' be generated using `make_default_request()`.
#' @returns A data frame with information about a party's sponsors.
#' @examples
#' \donttest{
#' \dontrun{
#' list_party_volumes() # Default is Rick Gilmore (party 6)
#' }
#' }
#' @export
list_party_volumes <- function(party_id = 6,
                               vb = FALSE,
                               rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id > 0)
  
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  # Handle NULL rq
  if (is.null(rq)) {
    if (vb) {
      message("NULL request object. Will generate default.")
      message("\nNot logged in. Only public information will be returned.")  
    }
    rq <- make_default_request()
  }
  
  party_info <- get_party_by_id(party_id, vb, rq)
  
  g <- get_party_by_id(party_id, vb, rq)
  
  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    purrr::map(
      g$access,
      .f = function(x) {
        as.data.frame(x[[3]])
      }
    ) |> purrr::list_rbind() |>
      dplyr::mutate(party_id = party_id, 
                    prename = g$prename,
                    sortname = g$sortname,
                    affiliation = g$affiliation)
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    g
  }
  #TODO: Reformat output
}
