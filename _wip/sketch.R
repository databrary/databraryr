source("R/CONSTANTS.R")

ROG_TEST_VOL <- 172
  
# Base request
rq <- httr2::request(DATABRARY_API) |>
  httr2::req_headers("Accept" = "application/json, text/plain, */*") |>
  httr2::req_user_agent("databraryr (https://databrary.github.io/databraryr)") |>
  httr2::req_retry(max_tries = RETRY_LIMIT) |>
  httr2::req_timeout(REQUEST_TIMEOUT) |>
  httr2::req_cookie_preserve(tempfile())

# login request
rq_login <- rq |>
  httr2::req_url(LOGIN) |>
  httr2::req_body_json(list(email = ROG_EMAIL, password = ROG_PW))

# logout request
rq_logout <- rq |>
  httr2::req_url(LOGOUT)
  
# perform login
rsp_login <- httr2::req_perform(rq_login)

# perform_logout 
rsp_logout <- httr2::req_perform(rq_logout)

# create slot request
rq_create_slot <- rq |>
  httr2::req_url(sprintf(CREATE_SLOT, ROG_TEST_VOL)) |>
  httr2::req_body_form(csverf = httr2::resp_body_json(rsp_login)$csverf)

# perform slot request
rsp_create_slot <- httr2::req_perform(rq_create_slot)
