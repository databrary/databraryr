#' List Sessions in a Volume.
#' 
#' @description
#' `r lifecycle::badge("superseded")`
#' `list_sessions_in_volume()` has been superseded in list_sessions().
#' 
#' @param vol_id Target volume ID. Defaults to 1.
#' @param vb Provide verbose output. Default is FALSE.
#' @returns A data frame with information about the sessions in a volume.
#' @examples
#' \donttest{
#' list_sessions_in_volume() # Lists sessions in volume 1.
#' }
#' @export
list_sessions_in_volume <- function(vol_id = 1, vb = FALSE) {
  list_sessions(vol_id = vol_id, vb = vb)
}
