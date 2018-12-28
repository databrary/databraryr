#' Lists assets in a given Databrary volume and session (slot).
#'
#' @param session_id Slot/session ID.
#' @param vol_id Selected volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the assets in the selected volume and session.
#' @examples
#' list_assets()
#' @export
list_assets_json <- function(session_id = 9825, vol_id = 75,
                             vb = FALSE) {
  # Error handling
  if (length(session_id) > 1) {
    stop("session_id must have length 1.")
  }
  if ((!is.numeric(session_id)) || session_id <= 0 ) {
    stop("session_id must be > 0.")
  }
  if (length(vol_id) > 1) {
    stop("vol_id must have length 1.")
  }
  if ((!is.numeric(vol_id)) || vol_id <= 0 ) {
    stop("vol_id must be > 0.")
  }
  if (!is.logical(vb)) {
    stop("vb must be a logical value.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }

  # if ((!exists("databrary_config_status")) || (!databrary_config_status)) {
  #   config_db(vb=vb)
  # }

  # Make URL, GET(), and handle response
  slot.url <- paste0("https://nyu.databrary.org/api/volume/", vol_id, "/slot/", session_id, "?assets")
  if (vb) {
    message(paste0("Sending GET to ", slot.url))
  }
  g <- httr::GET(slot.url)
  if (httr::status_code(g) == 200) {
    return(httr::content(g, 'text', encoding = 'UTF-8'))
  } else {
    if (vb) {
      message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
    }
  return(NULL)
  }
}
