#' Shows release level codes
#'
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return Data frame with release levels.
#' @examples
#' get_release_levels()
#' @export
get_release_levels <- function(vb = FALSE) {
  c <- assign_constants(vb = vb)
  rl <- c$release
  return(rl)
}
