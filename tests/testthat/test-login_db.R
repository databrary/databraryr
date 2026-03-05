test_that("login_db rejects bad input parameters", {
  expect_error(login_db(vb = -1))
  expect_error(login_db(vb = 3))
  expect_error(login_db(vb = "a"))
})

test_that("login_db stores token bundle on success", {
  orig <- get("oauth_password_grant", envir = asNamespace("databraryr"))
  assignInNamespace("oauth_password_grant", function(username, password, client_id, client_secret, vb = FALSE) list(access_token = "abc", refresh_token = "def", expires_in = 3600), ns = "databraryr")
  on.exit(assignInNamespace("oauth_password_grant", orig, ns = "databraryr"), add = TRUE)
  clear_token_bundle()
  expect_true(login_db(email = "user@example.com",
                       password = "pw",
                       client_id = "cid",
                       client_secret = "sec",
                       store = FALSE,
                       vb = FALSE))
  bundle <- get_token_bundle()
  expect_equal(bundle$access_token, "abc")
  expect_equal(bundle$refresh_token, "def")
  clear_token_bundle()
})

