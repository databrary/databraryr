#' List vol.id owners.
#'
#' @param this_vol_id Selected volume number. Default is 2.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return A data framw with information about the volume owner(s).
#' @examples
#' list_volume_owners()
#' @export
list_volume_owners <- function(this_vol_id = 1,
                               vb = FALSE) {
  # Error checking---------------------------------------------------------
  if (length(this_vol_id) > 1) {
    stop("'vol_id' must have length 1.")
  }
  if ((!is.numeric(this_vol_id)) || (this_vol_id <= 0)) {
    stop("'vol_id' must be an integer > 0.")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be logical.")
  }

  # Main body-------------------------------------------------------------
  v <- list_containers_records(vol_id = this_vol_id, vb = vb)
  
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
    p <- dplyr::mutate(p, vol_id = this_vol_id)
    p <- dplyr::rename(p, person_id = id)
    p <- dplyr::filter(p, !(is.na(prename)),
                       !(stringr::str_detect(prename, "Databrary")))
    p <- dplyr::select(p, vol_id, person_id, sortname, prename)
    return(p)
  } else {
    return(NULL)
  }
}
