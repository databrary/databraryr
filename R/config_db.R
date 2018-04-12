#' Configure R environment for access to Databrary.org.
#'
#' @param vb A boolean value.
#' @examples
#' configure_db()
config_db <- function(vb=FALSE) {
  if(!exists("databrary_config_status")){
    if (vb) message("Configuring for Databrary.\n")

    assign('databrary.url', 'https://nyu.databrary.org', envir=.GlobalEnv)
    assign('databrary_config_status', 1, envir=.GlobalEnv)
    assign('vol.api.url', "https://nyu.databrary.org/api/volume", envir=.GlobalEnv)
    httr::set_config(httr::add_headers(.headers = c("X-Requested-With" = "Databrary R client")))

    kl <- keyring::key_list(service = "databrary")
    if (exists('kl') && is.data.frame(kl)) {
      assign('system.credentials', TRUE, envir=.GlobalEnv)
      if (vb) message("Using stored system credentials file.")
    } else {
      assign('system.credentials', FALSE, envir=.GlobalEnv)
      message("No system credentials file exists. Create one with config_passwd()")
    }
    message("\nConfigured for Databrary access. Please log in using login_db()")
  } else if (vb) message("Already configured.\n")
}
