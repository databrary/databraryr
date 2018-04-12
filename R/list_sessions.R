#' Downloads session spreadsheet as a CSV.
#'
#' @param volume Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_sessions()
list_sessions <- function(volume = 1, vb = FALSE) {
  # Error checking
  if (!is.numeric(volume)) {
    stop("Volume ID must be numeric.")
  }
  if (volume < 1) {
    stop("Volume ID must be >= 1.")
  }
  if ((!exists("databrary_config_status")) || (!databrary_config_status)){
    databraryapi::config_db(vb = vb)
  }

  url.cont <- paste(vol.api.url, "/", volume, "?", "containers", sep="")
  if (vb) message(paste0("Sending GET to ", url.cont))
  g = httr::GET(url.cont)
  if (httr::status_code(g) == 200) {
    g.content <- httr::content(g, 'text', encoding = "UTF-8")
    v <- jsonlite::fromJSON(g.content)
    if (!is.null(v)) {
      if (("containers" %in% names(v)) && (!is.null(v[['containers']]))) {
        # Drop first element (contains metadata)
        return(v$containers[-1,])
      } else if (vb) {
        message(paste0('No sessions in volume.\n'))
      }
    }
  } else if (vb) message( paste( 'Download Failed, HTTP status ', httr::status_code(g), '\n', sep="" ) )
}
