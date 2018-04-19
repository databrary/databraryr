#' List volume owners.
#'
#' @param volume Selected volume number. Default is 2.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return A data framw with information about the volume owner(s).
#' @examples
#' list_volume_owners()
list_volume_owners <- function(volume = 1,
                               vb = FALSE) {
  # Error handling
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || (volume <= 0)) {
    stop("Volume must be an integer > 0.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  v <- download_containers_records(volume = volume, vb = vb)
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
    p <- dplyr::mutate(p, person.id = id, volume = volume)
    p <- dplyr::filter(p, !(is.na(prename)))
    p <- dplyr::select(p, volume, person.id, sortname, prename)
    return(p)
  } else {
    return(NULL)
  }
}
