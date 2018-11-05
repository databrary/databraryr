#' Lists all the assets/files in a given volume.
#'
#' @param vol.id Target volume number.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_assets_in_volume()
#' @export
list_assets_in_volume <- function(vol.id = 1, vb = FALSE) {
  # Error handling
  if (!is.numeric(vol.id)) {
    stop("Volume must be numeric.")
  }
  if (vol.id < 1) {
    stop("Volume must be >= 1.")
  }

  sl <- list_sessions_in_volume(vol.id = vol.id, vb = vb)
  if (!is.null(sl)) {
    if (vb) message("Session data exists.")
    s.ids <- sl$containers$id
    a <- lapply(s.ids, list_assets_in_session, vol.id = vol.id, vb = vb)
    a <- plyr::rbind.fill(a)
    return(a)
  } else {
    message(paste0("Session list for volume ", vol.id, " unavailable."))
    return(NULL)
  }
}
