#' List vol.id owners.
#'
#' @param vol_id Selected volume number. Default is 2.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return A data framw with information about the volume owner(s).
#' @examples
#' list_volume_owners()
#' @export
list_volume_owners <- function(vol_id = 1,
                               vb = FALSE) {
  # Error checking---------------------------------------------------------
  if (length(vol_id) > 1) {
    stop("'vol_id' must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("'vol_id' must be an integer > 0.")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be logical.")
  }

  # Main body-------------------------------------------------------------
  # v <- download_containers_records(vol_id = vol_id, vb = vb)
  v <- list_containers_records(vol_id = vol_id, vb = vb)
  if (!is.null(v$owners)) {
    owners <- v$owners$id
    if (length(owners) > 1) {
      l <- lapply(owners, download_party, vb = vb)
      Reduce(function(x,y) merge(x, y, all=TRUE), l) ->
        p
    } else {
      p <- as.data.frame(download_party(owners, vb = vb))
    }
    # Drop "Staff" etc.
    p <- dplyr::mutate(p, person_id = id, vol_id = vol_id)
    p <- dplyr::filter(p, !(is.na(prename)))
    p <- dplyr::select(p, vol_id, person_id, sortname, prename)
    return(p)
  } else {
    return(NULL)
  }
}
