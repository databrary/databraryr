source("R/CONSTANTS.R")

this_rq <- make_default_request()

databrary_error_body <- function(resp) {
  paste0(resp$method, " | ", resp$status)
}

this_rq <- rq %>%
    httr2::req_url(sprintf(DOWNLOAD_FILE, 1, 9807)) %>%
    httr2::req_progress()

try
resp <- httr2::req_perform(this_rq)
  
