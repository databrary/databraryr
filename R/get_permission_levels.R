#' Extracts Databrary permission level codes and labels.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' get_permission_levels()
get_permission_levels <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  pl <- c$permission
  return(pl)
}
