#' Lists a party's individual sponsors (people).
#'
#' @param party_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A tibble (data.frame) with the requested data.
#' @export
get_individual_sponsors <- function(party_id = 4210, vb = FALSE) {
  if (length(party_id) > 1) {
    stop("'party_id' must have length == 1.")
  }
  if (!is.numeric(party_id)) {
    stop("'party_id' must be an integer.")
  }
  if (party_id < 0) {
    stop("'party_id' must be > 0.")
  }

  if (length(vb) > 1) {
    stop("'vb' must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be a Boolean.")
  }

  return_val <- NULL

  sponsors <- get_sponsors(party_id = party_id, vb = vb)
  if (!is.null(sponsors)) {
    if (vb)
      message("Sponsor data exists.")
    if ("institution" %in% names(sponsors)) {
      if (vb)
        message("Possible institutional sponsors. Filtering out.")
      indiv_sponsors <- dplyr::filter(sponsors, is.na(institution))
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
