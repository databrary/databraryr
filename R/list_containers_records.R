#' List Containers and Records In Databrary Volume.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated and may be removed in a future release.
#' See `get_volume_by_id()` for related functionality.
#'
#' @param vol_id Databrary volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#'
#' @returns A list with containers and records from the specified volume.
#'
#' @examples
#' \donttest{
#' list_containers_records() # Containers and records from volume 1.
#' #' }
#'
#' @export
list_containers_records <- function(vol_id = 1,
                                    vb = FALSE) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  r <-
    GET_db_contents(
      URL_components = paste0('/api/volume/', vol_id,
                              '?containers&records'),
      vb = vb
    )
  return(r)
}
