#' List Sponsors For A Party
#'
#' @param party_id Target party ID.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A tibble (data.frame) with information about a party's sponsors.
#' @examples
#' \dontrun{
#' list_sponsors() # Default is Rick Gilmore (party 6)
#' }
#' @export
list_sponsors <- function(party_id = 6, vb = FALSE) {
  
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  if (vb)
    message(paste0("Getting sponsors for party ", party_id, "."))
  g <-
    databraryr::GET_db_contents(
      URL_components = paste0("/api/party/", party_id,
                              "?parents&children&access"),
      vb = vb
    )

  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    p <- g$parents$party
    if (!is.null(p)) {
      p
    } else {
      NULL
    }
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
