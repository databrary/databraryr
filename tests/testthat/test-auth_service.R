test_that("httr2_error_message handles missing and successful responses", {
  expect_match(databraryr:::httr2_error_message(NULL), "Request failed")

  ok_resp <- httr2::response(
    status_code = 200,
    url = "https://example.org/ok",
    body = raw()
  )
  expect_null(databraryr:::httr2_error_message(ok_resp))
})

test_that("httr2_error_message extracts error details", {
  error_resp <- httr2::response(
    status_code = 401,
    url = "https://example.org/error",
    headers = list("Content-Type" = "application/json"),
    body = charToRaw('{"error":"invalid_grant"}')
  )

  expect_match(databraryr:::httr2_error_message(error_resp), "HTTP 401")
})

test_that("oauth_password_grant returns NULL when request fails", {
  old_url <- get("OAUTH_TOKEN_URL", envir = asNamespace("databraryr"))
  on.exit(assignInNamespace("OAUTH_TOKEN_URL", old_url, ns = "databraryr"), add = TRUE)

  assignInNamespace("OAUTH_TOKEN_URL", "http://127.0.0.1:9/o/token/", ns = "databraryr")

  result <- databraryr:::oauth_password_grant(
    username = "user@example.org",
    password = "secret",
    client_id = "cid",
    client_secret = "csec",
    vb = FALSE
  )

  expect_null(result)
})

test_that("oauth_refresh_grant returns NULL when request fails", {
  databraryr:::set_token_bundle(access_token = "token", refresh_token = "refresh", expires_in = 3600)
  on.exit(databraryr:::clear_token_bundle(), add = TRUE)
  old_url <- get("OAUTH_TOKEN_URL", envir = asNamespace("databraryr"))
  on.exit(assignInNamespace("OAUTH_TOKEN_URL", old_url, ns = "databraryr"), add = TRUE)

  assignInNamespace("OAUTH_TOKEN_URL", "http://127.0.0.1:9/o/token/", ns = "databraryr")

  result <- databraryr:::oauth_refresh_grant(
    refresh_token = "refresh",
    client_id = "cid",
    client_secret = "csec",
    vb = FALSE
  )

  expect_null(result)
})

