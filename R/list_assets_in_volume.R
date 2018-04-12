#' Lists all the assets/files in a given volume.
#'
#' @param volume Target volume number.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_assets_in_volume()
list_assets_in_volume <- function(volume = 1, vb = FALSE) {
  # Error handling
  if (!is.numeric(volume)) {
    stop("Volume must be numeric.")
  }
  if (volume < 1) {
    stop("Volume must be >= 1.")
  }

  sl <- databraryapi::list_sessions(volume = volume, vb = vb)
  a <- sapply(as.array(sl[,'id']), databraryapi::list_assets, volume = volume, vb = vb)
  a <- Reduce(function(x, y) merge(x, y, all = TRUE), a)
  return(a)
}
