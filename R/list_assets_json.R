#' Lists assets in a given Databrary volume and session (slot).
#'
#' @param slot Slot/session ID.
#' @param volume Selected volume number.
#' @param convert.JSON A Boolean value. If TRUE converts JSON to data frame.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the assets in the selected volume and session.
#' @examples
#' list_assets()
list_assets_json <- function(slot = 9825, volume = 75,
                             vb = FALSE) {
  # Error handling
  if (length(slot) > 1) {
    stop("Slot must have length 1.")
  }
  if ((!is.numeric(slot)) || slot <= 0 ) {
    stop("Slot must be > 0.")
  }
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || volume <= 0 ) {
    stop("Volume must be > 0.")
  }

  if ((!exists("databrary_config_status")) || (!databrary_config_status)) {
    config_db(vb=vb)
  }

  # Make URL, GET(), and handle response
  slot.url <- paste0(vol.api.url, "/", volume, "/slot/", slot, "?assets")
  if (vb) {
    message(paste0("Sending GET to ", slot.url, "\n"))
  }
  g <- httr::GET(slot.url)
  if (httr::status_code(g) == 200) {
    return(httr::content(g, 'text', encoding = 'UTF-8'))
  } else {
    if (vb) {
      message(paste0( 'Download Failed, HTTP status ', httr::status_code(g), '\n'))
    }
  return(NULL)
  }
}
