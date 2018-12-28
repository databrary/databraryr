#' Downloads list of sessions.
#'
#' @param vol_id Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @return List of sessions.
#' @examples
#' list_sessions()
#' @export
list_sessions <- function(vol_id = 1, vb = FALSE) {
  # Error checking
  if (!is.numeric(vol_id)) {
    stop("vol_id must be numeric.")
  }
  if (vol_id < 1) {
    stop("vol_id must be >= 1.")
  }
  if (length(vol_id) > 1) {
    stop("vol_id must be single value.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  url.cont <- paste0("https://nyu.databrary.org/api/volume/", vol_id, "?", "containers")
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
        df$vol_id <- vol_id
        dplyr::rename(df, session_id = id)
        return(df)
      } else if (vb) {
        message(paste0('No sessions in volume.\n'))
      }
    }
  } else if (vb) message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
}
