# OAuth2 network operations ---------------------------------------------------

httr2_error_message <- function(resp) {
  if (is.null(resp)) {
    return("Request failed before receiving a response.")
  }
  status <- httr2::resp_status(resp)
  if (status < 400) {
    return(NULL)
  }
  body <- try(httr2::resp_body_json(resp), silent = TRUE)
  if (!inherits(body, "try-error") && is.list(body)) {
    fields <- c(body$error_description, body$error, body$detail)
    fields <- fields[!vapply(fields, is_missing_string, logical(1))]
    if (length(fields)) {
      return(paste0("HTTP ", status, ": ", fields[[1]]))
    }
  }
  sprintf("HTTP %s returned with empty error body.", status)
}

#' @noRd
oauth_password_grant <- function(username,
                                 password,
                                 client_id,
                                 client_secret,
                                 vb = FALSE) {
  assertthat::assert_that(assertthat::is.string(username))
  assertthat::assert_that(assertthat::is.string(password))
  assertthat::assert_that(assertthat::is.string(client_id))
  assertthat::assert_that(assertthat::is.string(client_secret))

  req <- make_default_request(with_token = FALSE) |>
    httr2::req_url(OAUTH_TOKEN_URL)

  resp <- tryCatch(
    req |>
      httr2::req_body_form(
        grant_type = "password",
        username = username,
        password = password,
        client_id = client_id,
        client_secret = client_secret
      ) |>
      httr2::req_perform(),
    error = function(err) {
      if (vb) message("OAuth token request failed: ", conditionMessage(err))
      NULL
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  if (httr2::resp_status(resp) >= 400) {
    if (vb) message(httr2_error_message(resp))
    return(NULL)
  }

  payload <- httr2::resp_body_json(resp)
  list(
    access_token = payload$access_token,
    refresh_token = if (is.null(payload$refresh_token)) NULL else payload$refresh_token,
    expires_in = if (is.null(payload$expires_in)) 3600 else payload$expires_in
  )
}

#' @noRd
oauth_refresh_grant <- function(refresh_token,
                                client_id,
                                client_secret,
                                vb = FALSE) {
  assertthat::assert_that(assertthat::is.string(refresh_token))
  assertthat::assert_that(assertthat::is.string(client_id))
  assertthat::assert_that(assertthat::is.string(client_secret))

  req <- make_default_request() |>
    httr2::req_url(OAUTH_TOKEN_URL)

  resp <- tryCatch(
    req |>
      httr2::req_body_form(
        grant_type = "refresh_token",
        refresh_token = refresh_token,
        client_id = client_id,
        client_secret = client_secret
      ) |>
      httr2::req_perform(),
    error = function(err) {
      if (vb) message("OAuth refresh request failed: ", conditionMessage(err))
      NULL
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  if (httr2::resp_status(resp) >= 400) {
    if (vb) message(httr2_error_message(resp))
    return(NULL)
  }

  payload <- httr2::resp_body_json(resp)
  list(
    access_token = payload$access_token,
    refresh_token = if (is.null(payload$refresh_token)) refresh_token else payload$refresh_token,
    expires_in = if (is.null(payload$expires_in)) 3600 else payload$expires_in
  )
}
