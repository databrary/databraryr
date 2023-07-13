#' List Owners of a Databrary Volume.
#'
#' @param this_vol_id Selected volume number. Default is volume 1.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about a volume's owner(s).
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
      Reduce(function(x, y)
        merge(x, y, all = TRUE), l) ->
        p
    } else {
      p <- as.data.frame(download_party(owners, vb = vb))
    }
    if (!is.null(p)) {
      p |>
        dplyr::mutate(vol_id = this_vol_id) |>
        dplyr::rename(person_id = "id") |>
        dplyr::filter(!(is.na(.data$prename)), !(stringr::str_detect(.data$prename, "Databrary"))) |>
        dplyr::select("vol_id", "person_id", "sortname", "prename")
    } else {
      if (vb) message("NULL value returned from `download_party()`.")
      p
    }
  } else {
    NULL
  }
}
