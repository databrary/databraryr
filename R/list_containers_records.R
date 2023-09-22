#' List Containers and Records In Databrary Volume.
#'
#' @param vol_id Databrary volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A list with containers and records from the specified volume.
#' @examples
#' list_containers_records() # Containers and records from volume 1.
#' @export
list_containers_records <- function(vol_id = 1,
                                    vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

    # Error handling
  if (length(vol_id) > 1) {
    stop("vol_id must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("vol_id must be an integer > 0.")
  }
  if (!is.logical(vb)) {
    stop("vb type must be logical.")
  }
  if (length(vb) > 1) {
    stop("vb must have length = 1.")
  }
  
  r <-
    GET_db_contents(
      URL_components = paste0('/api/volume/', vol_id,
                              '?containers&records'),
      vb = vb
    )
  return(r)
}
