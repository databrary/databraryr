#' Lists Keywords And Tags For A Volume.
#'
#' @param vol_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A data frame with the requested data.
#' @examples
#' \dontrun{
#' list_volume_tags()
#' }
#' @export
list_volume_tags <- function(vol_id = 1, vb = FALSE) {
  
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  g <-
    databraryr::GET_db_contents(URL_components = paste0("/api/volume/", vol_id,
                                                        "?tags=all"), vb = vb)
  if (!is.null(g)) {
    if (vb)
      message("n=", length(g$tags$id), " tags found in volume ", vol_id, ".")
    g$tags
  } else {
    if (vb)
      message("No tags available for volume '", vol_id, "'.")
    NULL
  }
}
