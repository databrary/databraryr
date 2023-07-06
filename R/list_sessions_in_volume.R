#' Lists the sessions in a given volume. This is an alias for list_sessions().
#'
#' @param vol_id Target volume ID. Defaults to 1.
#' @param vb A boolean value.
#' @return List of sessions.
#' @examples
#' list_sessions_in_volume() # Lists sessions in volume 1.
#' @export
list_sessions_in_volume <- function(vol_id = 1, vb = FALSE) {
  message("list_sessions_in_volume() is deprecated. Use list_sessions() instead.")
  list_sessions(vol_id = vol_id, vb = vb)
}
