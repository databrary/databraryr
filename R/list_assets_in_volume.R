#' Lists all the assets/files in a given volume.
#'
#' @param volume Target volume number.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_assets_in_volume()
list_assets_in_volume <- function(volume = 1, vb = FALSE) {
  # Lists assets in a given volume by session
  sl <- list_sessions(volume)
  a <- sapply(as.array(sl[,'id']), list_assets, volume = volume)
  a <- Reduce(function(x, y) merge(x, y, all = TRUE), a)
  return(a)
}
