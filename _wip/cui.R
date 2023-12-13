#https://developer.mozilla.org/en-US/docs/Web/HTTP/Cookies

#https://gist.github.com/jeffacce/b220aa79ef6aeda4caf0481073fab9e3

DATABRARY_API <- "https://nyu.databrary.org/api"
LOGIN <- "https://nyu.databrary.org/api/user/login"
LOGOUT <- "https://nyu.databrary.org/api/user/logout"
GET_VOL_BY_ID <-
  "https://nyu.databrary.org/api/volume/%s?access&citation&links&funding&top&tags&excerpts&comments&records&containers=all&metrics&state"
CREATE_SLOT <-
  "https://nyu.databrary.org/api/volume/{volumeid}/slot"
CREATE_UPLOAD_FLOW <-
  "https://nyu.databrary.org/api/volume/{volumeid}/upload"
UPLOAD_CHUNK <- "https://nyu.databrary.org/api/upload"
CREATE_FILE_FROM_FLOW <-
  "https://nyu.databrary.org/api/volume/{volumeid}/asset"
UPDATE_SLOT <- "https://nyu.databrary.org/api/slot/{slotid}"
QUERY_SLOT <-
  "https://nyu.databrary.org/api/slot/{slotid}/-?records&assets&excerpts&tags&comments"

USER_AGENT <-
  "databraryr (https://cran.r-project.org/package=databraryr)"
KEYRING_SERVICE <- 'org.databrary.databraryr'

RETRY_LIMIT <- 3
RETRY_WAIT_TIME <- 1  # seconds
RETRY_BACKOFF <- 2  # exponential backoff
REQUEST_TIMEOUT <- 5 # seconds

ROG_PRIVATE_VOL <- 106

create_session <-
  function(email = NULL,
           vb = TRUE,
           SERVICE = 'org.databrary.databraryr') {
    if (is.null(email)) {
      email <- Sys.getenv("DATABRARY_LOGIN")
    }
    assertthat::is.string(email)
    
    if (vb)
      message("Retrieving stored password")
    password <-
      try(keyring::key_get(service = SERVICE, username = email),
          silent = TRUE)
    if ("try-error" %in% class(password)) {
      if (vb)
        message("No password found in keyring for service='", SERVICE, "'.")
    } else {
      r <- NULL
      if (vb)
        message("Logging in...")
      r <- httr::POST(LOGIN,
                      body = list(email = email, password = password))
    }
    r
  }

create_client <-
  function(email = NULL,
           vb = TRUE,
           SERVICE = 'org.databrary.databraryr') {
    if (is.null(email)) {
      email <- Sys.getenv("DATABRARY_LOGIN")
    }
    assertthat::is.string(email)
    
    if (vb)
      message("Retrieving stored password")
    password <-
      try(keyring::key_get(service = SERVICE, username = email),
          silent = TRUE)
    if ("try-error" %in% class(password)) {
      if (vb)
        message("No password found in keyring for service='", SERVICE, "'.")
    } else {
      r <- NULL
      if (vb)
        message("Logging in...")
      r <- httr::POST(LOGIN,
                      body = list(email = email, password = password))
      if (httr::status_code(r) != 200) {
        if (vb)
          message("GET returns with status code: ", httr::status_code(r))
        return(NULL)
      }
    }
    r$session_id <- httr::cookies(r)$value
    r$csrf_token <-
      httr::content(r, type = "application/json")$csverf
    r
  }

login2 <- function() {
  req <- make_login_req()
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

logout2 <- function() {
  req <- make_logout_req()
  
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#https://httr2.r-lib.org/articles/wrapping-apis.html

get_volume_by_id <- function(volume_id = 8,
                             cookie_path = NULL,
                             vb = FALSE) {
  if (is.null(cookie_path)) {
    message("NULL value for `cookie_path`")
    cookie_path <- tempfile()
  }
  
  req <- httr2::request(sprintf(GET_VOL_BY_ID, volume_id)) |>
    httr2::req_retry(max_tries = RETRY_LIMIT) |>
    httr2::req_timeout(REQUEST_TIMEOUT) |>
    httr2::req_cookie_preserve(cookie_path)
  
  resp <- try(httr2::req_perform(req), silent = TRUE)
  if ("try-error" %in% class(resp)) {
    if (vb)
      message("Volume not available: ", volume_id)
    NULL
  } else {
    httr2::resp_body_json(resp)
  }
}

make_request <- function() {
  httr2::request(DATABRARY_API) |>
    httr2::req_headers("Accept" = "application/json") |>
    httr2::req_user_agent("databraryr (https://databrary.github.io/databraryr)") |>
    httr2::req_retry(max_tries = RETRY_LIMIT) |>
    httr2::req_timeout(REQUEST_TIMEOUT)
}


make_login_req <- function(email = Sys.getenv("DATABRARY_LOGIN"),
                           password = keyring::key_get(service = 'org.databrary.databraryr', username = email),
                           test_req_only = FALSE) {
  assertthat::is.string(email)
  assertthat::is.string(password)
  assertthat::assert_that(is.logical(test_req_only))
  
  path <- tempfile()
  req <- httr2::request(LOGIN) |>
    httr2::req_body_json(list(email = email, password = password)) |>
    httr2::req_retry(max_tries = RETRY_LIMIT) |>
    httr2::req_timeout(REQUEST_TIMEOUT) |>
    httr2::req_cookie_preserve(path)
  
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) == 200) {
    path
  } else {
    NULL
  }
}

initialize_client <-
  function(email = Sys.getenv("DATABRARY_LOGIN"),
           password = keyring::key_get(service = 'org.databrary.databraryr', username = email),
           test_req_only = FALSE) {
    assertthat::is.string(email)
    assertthat::is.string(password)
    assertthat::assert_that(is.logical(test_req_only))
    
    req <- httr2::request(LOGIN)
    req <- req |>
      httr2::req_body_json(list(email = email, password = password)) |>
      httr2::req_retry(max_tries = RETRY_LIMIT) |>
      httr2::req_timeout(REQUEST_TIMEOUT)
    
    if (test_req_only) {
      httr2::req_dry_run(req)
    } else {
      httr2::req_perform(req)
    }
  }

make_logout_req <- function() {
  req <- httr2::request(LOGOUT) |>
    httr2::req_user_agent("databraryr (https://databrary.github.io/databraryr)") |>
    httr2::req_retry(max_tries = RETRY_LIMIT) |>
    httr2::req_timeout(REQUEST_TIMEOUT)
  req
}

get_session_by_id <- function(volume, session_id) {
  httr2::resp_body_json(volume)$containers
}

make_db_request <- function() {
  path <- tempfile()
  req <- httr2::request(DATABRARY_API) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_retry(max_tries = RETRY_LIMIT) |>
    httr2::req_timeout(REQUEST_TIMEOUT) |>
    httr2::req_cookie_preserve(path)
  
  req
}

make_perform_login_req <-
  function(req,
           email = Sys.getenv("DATABRARY_LOGIN"),
           password = keyring::key_get(service = 'org.databrary.databraryr',
                                       username = email)) {
    req <- req |>
      httr2::req_url(LOGIN) |>
      httr2::req_body_json(list(email = email, password = password))
    
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) == 200) {
      TRUE
    } else {
      NULL
    }
  }

new_get_volume_by_id <- function(req,
                                 volume_id = ROG_PRIVATE_VOL,
                                 vb = FALSE) {
  vol_req <- httr2::req_url(req, sprintf(GET_VOL_BY_ID, volume_id))
  
  resp <- try(httr2::req_perform(vol_req), silent = TRUE)
  if ("try-error" %in% class(resp)) {
    if (vb)
      message("Volume not available: ", volume_id)
    NULL
  } else {
    httr2::resp_body_json(resp)
  }
}

login_db2 <- function(req,
                      email = Sys.getenv("DATABRARY_LOGIN"),
                      password = keyring::key_get(service = KEYRING_SERVICE,
                                                  username = email)) {
  req <- req |>
    httr2::req_url(LOGIN) |>
    httr2::req_body_json(list(email = email, password = password))
  
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) == 200) {
    TRUE
  } else {
    NULL
  }
}

logout_db2 <- function(req) {
  logout_req <- httr2::req_url(req, LOGOUT)
  
  resp <- httr2::req_perform(logout_req)
  
  delete_cookie <- file.remove(logout_req$options$cookiefile)
  
  if ((httr2::resp_status(resp) == 200) & (delete_cookie)) {
    TRUE
  } else {
    FALSE
  }
}
