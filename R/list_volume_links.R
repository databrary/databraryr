#' Retrieves URL Links From A Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the requested data.
#' @examples
#' \donttest{
#' list_volume_links() # Links from volume 1
#' }
#' @export
list_volume_links <- function(vol_id = 1, vb = FALSE) {
  
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  g <-
    databraryr::GET_db_contents(URL_components = paste0("/api/volume/", vol_id,
                                                          "?links=all"),
                                  vb = vb)
  if (!is.null(g)) {
    tibble::tibble(vol_id = vol_id, link_name = g$links$head, url = g$links$url)
  } else {
    NULL
  }
}
