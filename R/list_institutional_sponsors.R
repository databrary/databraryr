#' List Institutional Sponsors For A Party.
#'
#' A party (person or institution) may have a sponsor that is another person
#' or an institution. This function lists the *institution(s)* that sponsor
#' a party.
#'
#' @param party_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @param report_target_party A Boolean value. Print info about the target party.
#' @returns A data frame with information about a party's *institutional* sponsor(s).
#' @examples
#' \dontrun{
#' list_institutional_sponsors() # Defaults to Rick Gilmore (party 6)
#' #' }
#' @export
list_institutional_sponsors <-
  function(party_id = 6,
           report_target_party = TRUE,
           vb = FALSE) {

    # Check parameters
    assertthat::assert_that(length(party_id) == 1)
    assertthat::assert_that(is.numeric(party_id))
    assertthat::assert_that(party_id >= 1)
    
    assertthat::assert_that(length(report_target_party) == 1)
    assertthat::assert_that(is.logical(report_target_party))
    
    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))
    
    if (length(party_id) > 1) {
      stop("party_id must have length == 1.")
    }
    if (!is.numeric(party_id)) {
      stop("party_id must be an integer.")
    }
    if (party_id < 0) {
      stop("party_id must be > 0.")
    }
    if (length(vb) > 1) {
      stop("vb must have length == 1.")
    }
    if (!is.logical(vb)) {
      stop("vb must be a logical value.")
    }
    
    if (report_target_party) {
      g <-
        databraryr::GET_db_contents(
          URL_components = paste0("/api/party/", party_id,
                                  "?parents&children&access"),
          vb = vb
        )
      message(
        "Institutional sponsors for party ",
        party_id,
        ", ",
        paste0(g$prename, " ", g$sortname), ":"
      )
    }
    
    sponsors <- list_sponsors(party_id = party_id, vb = vb)
    if (!is.null(sponsors)) {
      if (vb)
        message("Sponsor data exists.")
      if ("institution" %in% names(sponsors)) {
        if (vb)
          message("Possible institutional sponsors for party.")
        inst_sponsors <-
          dplyr::filter(sponsors, .data$institution == TRUE)
        if (is.null(inst_sponsors)) {
          if (vb)
            message(paste0("No institutional sponsors for party ", party_id))
        }
        inst_sponsors
      } else {
        if (vb)
          message(paste0("No institutional sponsors for party ", party_id))
        NULL
      }
    } else {
      if (vb)
        stop(paste0("No sponsors for party ", party_id))
      NULL
    }
  }
