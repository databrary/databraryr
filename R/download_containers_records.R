#' Downloads container and record structure from Databrary volume.
#' This function is deprecated, as it duplicates list_containers_records().
#'
#' @param vol_id Databrary volume number.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return List of containers and records from the specified volume.
#' @examples
#' download_containers_records()
#' @export
download_containers_records <- function(vol_id = 1,
                                        vb = FALSE) {

  list_containers_records(vol_id = vol_id, vb = vb)
}
