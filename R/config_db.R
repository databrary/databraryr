#' Configure R environment for access to Databrary.org.
#'
#' @param vb A boolean value.
#' @return Status code if successful.
#' @examples
#' configure_db()
config_db <- function(vb=FALSE) {
  # Determines if local environment has been configured for Databrary. If not,
  # sources required packages and commands and adds relevant defaults to .GlobalEnv
  #
  # Args:
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.

  if(!exists("databrary_config_status")){
    if (vb) cat("Configuring for Databrary.\n")

    assign('databrary.url', 'https://nyu.databrary.org', envir=.GlobalEnv)
    assign('databrary_config_status', 1, envir=.GlobalEnv)
    assign('vol.api.url', "https://nyu.databrary.org/api/volume", envir=.GlobalEnv)
    httr::set_config(httr::add_headers(.headers = c("X-Requested-With" = "databrary R client")))
    # TODO(someone): Check to see if there is a valid credentials file
  } else if (vb) cat("Already configured.\n")
}
