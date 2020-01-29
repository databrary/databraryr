#' Lists image or video excerpts associated with a Databrary volume.
#'
#' @param vol_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A tibble (data.frame) with the requested data.
#' @example
#' list_volume_excerpts() # Default is volume 1
#' @export
list_volume_excerpts <- function(vol_id = 1, vb = FALSE) {
  if (length(vol_id) > 1) {
    stop("'vol_id' must have length == 1.")
  }
  if (!is.numeric(vol_id)) {
    stop("'vol_id' must be an integer.")
  }
  if (vol_id < 0) {
    stop("'vol_id' must be > 0.")
  }

  if (length(vb) > 1) {
    stop("'vb' must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be a Boolean.")
  }

  g <-
    databraryapi::GET_db_contents(
      URL_components = paste0("/api/volume/", vol_id,
                              "?excerpts=all"),
      vb = vb
    )
  if (!is.null(g)) {
    if (vb)
      message("'tags' is NULL.")
    r <- g$excerpts
    if (!is.null(r)) {
      r
    } else {
      NULL
    }
  }
}
