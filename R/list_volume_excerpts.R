#' List Image or Video Excerpts On A Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @returns A data frame with information about any available excerpts.
#' @examples
#' \dontrun{
#' list_volume_excerpts()
#' }
#' @export
list_volume_excerpts <- function(vol_id = 1, vb = FALSE) {
  
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  g <-
    databraryr::GET_db_contents(
      URL_components = paste0("/api/volume/", vol_id,
                              "?excerpts=all"),
      vb = vb
    )
  if (!is.null(g)) {
    if (vb)
      message("'tags' is NULL.")
    r <- g$excerpts
    if (!is.null(r)) {
      r
    } else {
      NULL
    }
  }
}
