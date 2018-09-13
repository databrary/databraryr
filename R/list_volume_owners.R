#' List vol.id owners.
#'
#' @param vol.id Selected volume number. Default is 2.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return A data framw with information about the volume owner(s).
#' @examples
#' list_volume_owners()
#' @export
list_volume_owners <- function(vol.id = 1,
                               vb = FALSE) {
  # Error handling
  if (length(vol.id) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(vol.id)) || (vol.id <= 0)) {
    stop("Volume must be an integer > 0.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  v <- download_containers_records(vol.id = vol.id, vb = vb)
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
    p <- dplyr::mutate(p, person.id = id, vol.id = vol.id)
    p <- dplyr::filter(p, !(is.na(prename)))
    p <- dplyr::select(p, vol.id, person.id, sortname, prename)
    return(p)
  } else {
    return(NULL)
  }
}
