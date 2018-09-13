#' Downloads session spreadsheet as a CSV.
#'
#' @param volume Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_sessions()
#' @export
list_sessions <- function(volume = 1, vb = FALSE) {
  # Error checking
  if (!is.numeric(volume)) {
    stop("Volume ID must be numeric.")
  }
  if (volume < 1) {
    stop("Volume ID must be >= 1.")
  }
  if (length(volume) > 1) {
    stop("Volume ID must be single value.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  url.cont <- paste0("https://nyu.databrary.org/api/volume/", volume, "?", "containers")
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
  } else if (vb) message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
}
