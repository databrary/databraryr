#' List Owners of a Databrary Volume.
#'
#' @param vol_id Selected volume number. Default is volume 1.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about a volume's owner(s).
#' @examples
#' list_volume_owners()
#' @export
list_volume_owners <- function(vol_id = 1,
                               vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  v <- list_containers_records(vol_id = vol_id, vb = vb)
  
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
        dplyr::mutate(vol_id = vol_id) |>
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
