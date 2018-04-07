#' List volume owners.
#'
#' @param volume Selected volume number. Default is 2.
#' @return A data framw with information about the volume owner(s).
#' @examples
#' list_volume_owners()
list_volume_owners <- function(volume = 1) {
  # Error handling
  if (!is.numeric(volume)) {
    stop("Volume must be numeric")
  }
  v <- databraryapi::download_containers_records(volume)
  if (!is.null(v$owners)) {
    owners <- v$owners$id
    if (length(owners) > 1) {
      l <- lapply(owners, databraryapi::download_party)
      Reduce(function(x,y) merge(x, y, all=TRUE), l) ->
        p
    } else {
      p <- as.data.frame(databraryapi::download_party(owners))
    }
    # Drop "Staff" etc.
    p <- p %>%
      dplyr::mutate(., person.id = id, volume = volume) %>%
      dplyr::filter(., !(is.na(prename))) %>%
      dplyr::select(., volume, person.id, sortname, prename)
    return(p)
  } else {
    return(NULL)
  }
}
