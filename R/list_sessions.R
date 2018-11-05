#' Downloads session spreadsheet as a CSV.
#'
#' @param vol.id Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_sessions()
#' @export
list_sessions <- function(vol.id = 1, vb = FALSE) {
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
    if (vb) message("Successful retrieval.")
    g.content <- httr::content(g, 'text', encoding = "UTF-8")
    v <- jsonlite::fromJSON(g.content)
    if (!is.null(v)) {
      if (vb) message("Non-null content returned.")
      if (("containers" %in% names(v)) && (!is.null(v[['containers']]))) {
        # Drop first element (contains metadata)
        df <- v$containers[-1,]
        df$vol.id <- vol.id
        #dplyr::rename(df, session.id = id)
        # df <- dplyr::select(df, vol.id, id, top, name, date, release)
        # dplyr::rename(df, session.id = id)
        return(df)
      } else if (vb) {
        message(paste0('No sessions in volume.\n'))
      }
    }
  } else if (vb) message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
}
