#' Lists containers and record structure from Databrary volume.
#'
#' @param vol_id Databrary volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return JSON file with containers and records from the specified volume.
#' @examples
#' list_containers_records_json()
#' @export
list_containers_records <- function(vol_id = 1,
                                        vb = FALSE) {

  # Error handling
  if (length(vol_id) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("Volume must be an integer > 0.")
  }
  if (!is.logical(vb)) {
    stop("vb type must be logical.")
  }
  if (length(vb) > 1) {
    stop("vb must have length = 1.")
  }

  r <- GET_db_contents(URL_components = paste0('/api/volume/', vol_id,
                                               '?containers&records'),
                       vb = vb)
  return(r)
}
