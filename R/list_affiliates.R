#' List Affiliates for A Databrary Volume.
#'
#' @param party_id Target volume number.
#' @param report_target_party Report data about the target party. Default is FALSE.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about a party's affiliates (sponsored
#' investigators).
#' @examples
#' \donttest{
#' list_affiliates() # Lists Rick Gilmore's affiliates
#' #' }
#' @export
list_affiliates <- function(party_id = 6, report_target_party = FALSE,
                            vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id >= 1)
  
  assertthat::assert_that(length(report_target_party) == 1)
  assertthat::assert_that(is.logical(report_target_party))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  g <-
    databraryr::GET_db_contents(
      URL_components = paste0("/api/party/", party_id,
                              "?parents&children&access"),
      vb = vb
    )

  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    if (report_target_party) {
      message("Affiliates for party ", party_id, ", ", paste0(g$prename, " ", g$sortname), ", ", g$affiliation, ":")
    }
    p <- g$children$party
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
