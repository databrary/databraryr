#' @eval options::as_params()
#' @name options_params
#' 
NULL

#' Log Out of Databrary.org.
#'
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' 
#' @returns TRUE if logging out succeeds, FALSE otherwise.
#' 
#' @inheritParams options_params
#' 
#' @examples
#' \donttest{
#' logout_db()
#' }
#' @export
logout_db <- function(vb = options::opt("vb")) {
  validate_flag(vb, "vb")

  bundle <- get_token_bundle()
  if (is.null(bundle)) {
    if (vb) message("No active session; nothing to log out from.")
    return(TRUE)
  }

  clear_token_bundle()

  if (vb) message("Logout successful.")
  TRUE
}
