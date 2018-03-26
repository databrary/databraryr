#' Downloads session spreadsheet as a CSV.
#'
#' @param volume Target volume number.
#' @param vb A boolean value.
#' @return List of assets.
#' @examples
#' list_sessions()
list_sessions <- function(volume = 1, vb = vb) {
  v <- download_containers_records(volume = volume, vb = vb)
  if (!is.null(v)) {
    if (("containers" %in% names(v)) && (!is.null(v[['containers']]))) {
      # Drop first element (contains metadata)
      return(v$containers[-1,])
    } else if (vb) {
      cat(paste0('No containers in volume.\n'))
    }
  }
}
