#' Downloads list of sessions.
#'
#' @param vol_id Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @return List of sessions.
#' @examples
#' list_sessions() # Lists sessions in volume 1.
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
  
  r <- GET_db_contents(URL_components = paste0('/api/volume/', vol_id, '?containers'))
  if (!is.null(r)) {
    if (vb) message(" Non-null content returned.")
    if (("containers" %in% names(r)) && (!is.null(r[['containers']])) &&
        (dim(r[['containers']])[1] > 1)) {
      # Drop first element (contains metadata)
      if (vb) message(" Dropping metadata session and renaming vars.")
      df <- r$containers[-1,]
      if (dim(df)[1] >= 1) {
        df$vol_id <- vol_id
        df <- dplyr::rename(df, session_id = id)
        df        
      } else {
        # After removing sessions folder, no other data remains
        NULL
      }
    } else {
      if (vb) message(paste0('No sessions in volume.\n'))
      NULL
    }
    #return(r)
  } else {
    if (vb) message(' Null content returned.')
    NULL
  }
}
