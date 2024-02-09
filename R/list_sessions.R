#' Download Information About A Volume or Volume's Sessions.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' `list_sessions()` has been deprecated in favor of `list_volume_sessions()`.
#'
#' @param vol_id Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @returns Information about sessions in a volume.
#' @examples
#' \donttest{
#' list_sessions() # Lists sessions in volume 1.
#' }
#' @export
list_sessions <- function(vol_id = 1, vb = FALSE) {
  
  #------------------------------------------------------------
  # Check parameters
  
  assertthat::is.number(vol_id)
  assertthat::assert_that(sum(vol_id >= 1) == length(vol_id))
  assertthat::assert_that(!is.logical(vol_id))
  assertthat::assert_that(!is.character(vol_id))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  #------------------------------------------------------------
  # Helper function
  
  list_one_session <- function(vol_id = NULL, vb = NULL) {
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
          
          session_id <- NULL
          top <- NULL
          date <- NULL
          release <- NULL
          name <- NULL
          
          df <- dplyr::rename(df, session_id = "id") %>%
            dplyr::select(vol_id, session_id, top, date, release)
          # Not all sessions have a `name` field
          if ("name" %in% names(df)) {
            df <- df %>%
              dplyr::mutate(name = name)
          } else {
            df$name = NA
          }
          df
        } else {
          # After removing sessions folder, no other data remains
          NULL
        }
      } else {
        if (vb) message(paste0('No sessions in volume.\n'))
        NULL
      }
    } else {
      if (vb) message(' Null content returned.')
      NULL
    }
  }
  
  #------------------------------------------------------------
  # Map across volumes
  
  if (vb) message("Retrieving sessions for n= ", length(vol_id), " volumes.")
  
  purrr::map(vol_id, list_one_session, vb = vb) %>%
    purrr::list_rbind()
}
