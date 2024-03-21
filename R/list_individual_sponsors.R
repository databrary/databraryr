#' List A Party's Individual Sponsors.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated and may be removed in a future release.
#' See `list_party_sponsors()` for similar functionality.
#'
#' A party (person or institution) may have a sponsor that is another person
#' or an institution. This function lists the *person(s)* who sponsor
#' a party.
#'
#' @param party_id Party number. Default is 406 (Kasey Soska)
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param rq An `httr2` request object. If NULL (the default)
#' a request will be generated, but this will only permit public information
#' to be returned.
#'
#' @returns A data frame with information about a party.
#'
#' @examples
#' \donttest{
#' \dontrun{
#
#' list_individual_sponsors() # Default is Kasey Soska (party 406)
#'
#' }
#' }
#'
#' @export
list_individual_sponsors <- function(party_id = 406,
                                     vb = FALSE,
                                     rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  # Handle NULL request
  if (is.null(rq)) {
    if (vb) {
      message("NULL request object. Will generate default.")
      message("Not logged in. Only public information will be returned.")  
    }
    rq <- databraryr::make_default_request()
  }
  if (vb)
    message(paste0("Retrieving sponsors for party ", party_id, "."))
  
  party <- NULL
  party <- databraryr::get_party_by_id(party_id, vb, rq)
  
  if (!is.null(party)) {
    party.institution <- NULL
    df <- purrr::map(party$parents, as.data.frame) %>%
      purrr::list_rbind()
    if ("party.institution" %in% names(df)) {
      dplyr::filter(df, is.na(party.institution))
    } else {
      df
    }
  } else {
    party
  }
}