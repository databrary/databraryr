#' Lists the sessions in a given volume.
#'
#' @param vol.id Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @return List of sessions.
#' @examples
#' list_sessions_in_vol()
#' @export
list_sessions_in_volume <- function(vol.id = 1, vb = FALSE) {
  # Error checking
  if (!is.numeric(vol.id)) {
    stop("vol.id must be numeric.")
  }
  if (vol.id < 1) {
    stop("vol.id must be >= 1.")
  }
  if (length(vol.id) > 1) {
    stop("vol.id must be single value.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  url.cont <- paste0("https://nyu.databrary.org/api/volume/", vol.id, "?", "containers")
  if (vb) message(paste0("Sending GET to ", url.cont))
  g = httr::GET(url.cont)
  if (httr::status_code(g) == 200) {
    if (vb) message("Successful response. Extracting content.")
    g.content <- httr::content(g, 'text', encoding = "UTF-8")
    v <- jsonlite::fromJSON(g.content)
    if (!is.null(v)) {
      if (vb) message("Non-null data returned.")
      if (("containers" %in% names(v)) && (!is.null(v[['containers']]))) {
        return(v)
      } else if (vb) {
        message(paste0('No sessions in volume.\n'))
        return (NULL)
      }
    }
  } else if (vb) message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
}
