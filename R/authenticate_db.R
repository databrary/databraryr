#' Authenticate to Databrary.org.
#'
#' @param vb A boolean value.
#' @return Status code if successful.
#' @examples
#' authenticate_db()
authenticate_db <- function(vb = FALSE) {
  # Authenticates to Databrary.
  #
  # Args:
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.

  if (!exists("databrary_config_status")) {
    config_db(vb = vb)
  }

  if (".databrary.RData" %in% dir(all.files=TRUE)){
    load(".databrary.RData")

    # Don't print cookie to console via cat()
    sink("tmp")
    httr::set_config(httr::config(cookie = cat(paste0('session=\"', databrary.SESSION, '\"'))))
    sink()
    system("rm tmp")

    auth.url <- paste0(databrary.url, "/volume/1")
    r <- httr::GET(auth.url)
    if (vb) {
      cat(sprintf("\nGET command to %s\n", auth.url))
    }
    if (httr::status_code(r) != 200){
      if (vb) {
        cat(sprintf("\nStatus %i. Must login again.\n", httr::status_code(r)))
      }
      db_login(vb = vb)
    } else if(vb) {
      cat("\nAuthenticated to Databrary.\n")
    }
  } else {
    db_login(vb = vb)
  }
}
