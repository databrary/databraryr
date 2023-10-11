#' Download Information About A Volume's Sessions.
#'
#' @param vol_id Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @returns Information about sessions in a volume.
#' @examples
#' \dontrun{
#' list_sessions() # Lists sessions in volume 1.
#' #' }
#' @export
list_sessions <- function(vol_id = 1, vb = FALSE) {
  
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  r <- GET_db_contents(URL_components = paste0('/api/volume/', vol_id, '?containers'))
  if (!is.null(r)) {
    if (vb) message(" Non-null content returned.")
    if (("containers" %in% names(r)) && (!is.null(r[['containers']])) &&
        (dim(r[['containers']])[1] > 1)) {
      # Drop first element (contains metadata)
      if (vb) message(" Dropping metadata session and renaming vars.")
      df <- r$containers[-1,]
      if (dim(df)[1] >= 1) {
        df$vol_id <- vol_id
        df <- dplyr::rename(df, session_id = "id")
        df        
      } else {
        # After removing sessions folder, no other data remains
        NULL
      }
    } else {
      if (vb) message(paste0('No sessions in volume.\n'))
      NULL
    }
    #return(r)
  } else {
    if (vb) message(' Null content returned.')
    NULL
  }
}
