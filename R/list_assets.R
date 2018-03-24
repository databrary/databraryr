#' Plot summary of volume's participant characteristics.
#'
#' @param slot Slot/session ID.
#' @param volume Selected volume number.
#' @param return.response A Boolean value. If TRUE returns results from GET.
#' @param convert.JSON A Boolean value. If TRUE converts JSON to data frame.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A plot of a volume's summary demographic characteristics.
#' @examples
#' list_assets()
list_assets <- function(slot = 9825, volume = 75,
                        return.response = FALSE, convert.JSON = TRUE,
                        vb = FALSE) {
  # Lists the assets given a volume and slot (session).
  #
  # Args:
  #  slot: Databrary slot number (integer). Default is 9825.
  #  volume: Volume number to query. Default is 75.
  #  url.base: Base URL for API. Default is "https://nyu.databrary.org/api/volume"
  #  asset: Databrary asset number (integer). Default is 11643.
  #  return.response: Flag specifying whether to return the HTTP response. Default is FALSE.
  #  convert.JSON: Flag specifying whether to convert HTTP response to JSON. Default is TRUE.
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.
  #
  # Returns:
  #  A list of session assets and related metadata.
  #
  #  $id: slot/session number
  #  $top: ??
  #  $name: session name
  #  $assets (list)
  #    id, format, classification, duration, name, permission, size
  #
  #  So, x <- databrary_download_asset()
  #  x$assets[1] or x$assets['id'] gives array of asset ids

  # Error handling
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || volume <= 0 ) {
    stop("Volume must be > 0.")
  }
  if (length(slot) > 1) {
    stop("Slot must have length 1.")
  }
  if ((!is.numeric(slot)) || slot <= 0 ) {
    stop("Slot must be > 0.")
  }

  query.type <- "download"

  if ((!exists("databrary_config_status")) || (!databrary_config_status)) {
    config_db(vb=vb)
  }
  authenticate_db(vb=vb)

  slot.url <- paste0(databrary.url, "/", volume, "/slot/", slot, "?assets")
  if (vb) {
    cat(slot.url, "\n")
  }
  g <- httr::GET(slot.url)
  if (httr::status_code(g) == 200) {
    g.content <- httr::content(g, 'text', encoding = 'UTF-8')
    if(convert.JSON) {
      return(jsonlite::fromJSON(g.content))
      } else {
      return(g.content)
      }
  } else if (vb) {
    cat( paste( 'Download Failed, HTTP status ', httr::status_code(g), '\n', sep="" ) )
  }
}
