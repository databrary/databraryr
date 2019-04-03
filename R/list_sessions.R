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

  r <- GET_db_contents(URL_components = paste0('volume/', vol_id, '?containers'))
  if (!is.null(r)) {
    if (vb) message("Non-null content returned.")
    if (("containers" %in% names(r)) && (!is.null(r[['containers']]))) {
      # Drop first element (contains metadata)
      if (vb) message("Dropping metadata session and renaming vars.")
      df <- r$containers[-1,]
      df$vol_id <- vol_id
      df <- dplyr::rename(df, session_id = id)
      return(df)
    } else if (vb) {
      message(paste0('No sessions in volume.\n'))
    }
    return(r)
  } else {
    message('Null content returned.')
  }

  # url_cont <- paste0("https://nyu.databrary.org/api/volume/", vol_id, "?", "containers")
  # if (vb) message(paste0("Sending GET to ", url_cont))
  # g = httr::GET(url_cont)
  # if (httr::status_code(g) == 200) {
  #   if (vb) message("Successful retrieval.")
  #   g.content <- httr::content(g, 'text', encoding = "UTF-8")
  #   v <- jsonlite::fromJSON(g.content)
  #   if (!is.null(v)) {
  #     if (vb) message("Non-null content returned.")
  #     if (("containers" %in% names(v)) && (!is.null(v[['containers']]))) {
  #       # Drop first element (contains metadata)
  #       if (vb) message("Dropping metadata session and renaming vars.")
  #       df <- v$containers[-1,]
  #       df$vol_id <- vol_id
  #       df <- dplyr::rename(df, session_id = id)
  #       return(df)
  #     } else if (vb) {
  #       message(paste0('No sessions in volume.\n'))
  #     }
  #   }
  # } else if (vb) message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
}
