#' List A Party's Individual Sponsors.
#'
#' A party (person or institution) may have a sponsor that is another person
#' or an institution. This function lists the *person(s)* who sponsor
#' a party.
#' 
#' @param party_id Party number. Default is 406 (Kasey Soska)
#' @param report_target_party Print the party ID and name of the target person.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about a party.
#' @examples
#' \dontrun{
#' list_individual_sponsors() # Default is Kasey Soska (party 406)
#' }
#' @export
list_individual_sponsors <- function(party_id = 406, 
                                     report_target_party = FALSE,
                                     vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id >= 1)
  
  assertthat::assert_that(length(report_target_party) == 1)
  assertthat::assert_that(is.logical(report_target_party))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  return_val <- NULL

  if (vb)
    message(paste0("Retrieving sponsors for party ", party_id, "."))
  
  sponsors <- list_sponsors(party_id = party_id, vb = vb)
  
  if (report_target_party) {
    g <-
      databraryr::GET_db_contents(
        URL_components = paste0("/api/party/", party_id,
                                "?parents&children&access"),
        vb = vb
      )
    message("Sponsors for party ", party_id, ", ", paste0(g$prename, " ", g$sortname), ", ", g$affiliation, ":")
  }
  
  if (!is.null(sponsors)) {
    if (vb)
      message("Sponsor data exists.")
    if ("institution" %in% names(sponsors)) {
      if (vb)
        message("Possible institutional sponsors. Filtering out.")
      indiv_sponsors <- dplyr::filter(sponsors, is.na(sponsors$institution))
      if ((is.null(indiv_sponsors)) ||
          (nrow(indiv_sponsors) == 0)) {
        if (vb)
          message(paste0("No individual sponsors for party ", party_id, "."))
      } else {
        return_val <- indiv_sponsors
      }
    } else {
      if (vb)
        message(paste0("No institutional sponsors for party ", party_id))
      return_val <- sponsors
    }
    return_val
  } else {
    if (vb)
      stop(paste0("No sponsors for party ", party_id))
    return_val
  }
}
