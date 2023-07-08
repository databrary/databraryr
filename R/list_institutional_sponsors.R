#' Lists a party's institutional sponsors.
#'
#' @param party_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A tibble (data.frame) with the requested data.
#' @examples
#' list_institutional_sponsors() # Defaults to Rick Gilmore (party 6)
#' @export
list_institutional_sponsors <-
  function(party_id = 6, vb = FALSE) {
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
    
    sponsors <- list_sponsors(party_id = party_id, vb = vb)
    if (!is.null(sponsors)) {
      if (vb)
        message("Sponsor data exists.")
      if ("institution" %in% names(sponsors)) {
        if (vb)
          message("Possible institutional sponsors for party.")
        inst_sponsors <- dplyr::filter(sponsors, .data$institution == TRUE)
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
