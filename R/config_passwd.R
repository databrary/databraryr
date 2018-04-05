#' Stores Databrary account credentials in secure system-level file.
#'
#' @examples
#' config_passwd()
config_passwd <- function() {
  require(keyring)
  cat("Please enter your Databrary user account (email):\n")
  email <- readline(prompt="Email: ")
  kl <- key_list(service = "databrary")
  if (exists('kl') && is.data.frame(kl)) {
    if (email %in% kl$username) {
      cat(paste0("Databrary password exists for user: ", email, "\n"))
      enter.new <- readline(prompt = paste0("Enter new password for user: ", email, " (y/n)?: "))
      if (enter.new %in% c("Y", "y")) {
        keyring::key_set(service = "databrary", username = email)
        cat(paste0("New password saved for user: ", email, "\n"))
      } else {
        cat(paste0("Password unchanged for user: ", email, "\n"))
      }
      assign('system.credentials', TRUE, envir=.GlobalEnv)
    } else {
      cat(paste0("No Databrary password for user: ", email, "\n"))
      cat(paste0("Databrary passwords exist for other users: \n"))
      #cat(paste0(as.character(key_list(service = "databrary")[,2])))
      cat(pretty_print_emails(as.character(key_list(service = "databrary")[,2])))
      enter.new <- readline(prompt = paste0("Create new password for user: ", email, " (y/n)?: "))
      if (enter.new %in% c("Y", "y")) {
        keyring::key_set(service = "databrary", username = email)
        cat(paste0("New password saved for user: ", email, "\n"))
      } else {
        cat(paste0("Password unchanged for user: ", email, "\n"))
      }
    }
  } else {
    cat("No Databrary account passwords currently stored.\n")
    cat(paste0("Creating new password for user: ", email, "\n"))
    keyring::key_set(service="databrary", username=email)
    assign('system.credentials', TRUE, envir=.GlobalEnv)
  }
}

pretty_print_emails <- function(l) {
  s <- paste0("\t", l[1], "\n")
  if (length(l > 1)) {
    for (i in 2:length(l)) {
      s <- paste0(s, "\t", l[i], "\n")
    }
  }
  return(s)
}
