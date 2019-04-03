#' Lists all the assets/files in a given volume.
#'
#' @param vol_id Target volume number.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_assets_in_volume()
#' @export
list_assets_in_volume <- function(vol_id = 1, vb = FALSE) {
  # Error handling
  if (!is.numeric(vol_id)) {
    stop("Volume must be numeric.")
  }
  if (vol_id < 1) {
    stop("Volume must be >= 1.")
  }

  sl <- list_sessions_in_volume(vol_id = vol_id, vb = vb)
  if (!is.null(sl)) {
    if (vb) message("Session data exists.")
    #s_ids <- sl$containers$id
    s_ids <- sl$session_id
    a <- lapply(s_ids, list_assets_in_session, vol_id = vol_id, vb = vb)
    a <- plyr::rbind.fill(a)
    return(a)
  } else {
    message(paste0("Session list for volume ", vol_id, " unavailable."))
    return(NULL)
  }
}
