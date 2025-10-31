test_that("ensure_valid_token requires an existing bundle", {
  databraryr:::clear_token_bundle()
  expect_error(databraryr:::ensure_valid_token(), "No OAuth token available")
})

test_that("ensure_valid_token returns bundle when still valid", {
  databraryr:::clear_token_bundle()
  databraryr:::set_token_bundle(access_token = "still-valid", expires_in = NULL)

  bundle <- databraryr:::ensure_valid_token(refresh = TRUE)
  expect_equal(bundle$access_token, "still-valid")
})

test_that("ensure_valid_token errors when refresh not permitted", {
  databraryr:::clear_token_bundle()
  databraryr:::set_token_bundle(access_token = "expiring", refresh_token = "refresh", expires_in = -120)

  expect_error(databraryr:::ensure_valid_token(refresh = FALSE), "refresh disabled")
  databraryr:::clear_token_bundle()
})

test_that("ensure_valid_token errors when refresh token missing", {
  databraryr:::clear_token_bundle()
  databraryr:::set_token_bundle(access_token = "expiring", refresh_token = NULL, expires_in = -120)

  expect_error(databraryr:::ensure_valid_token(), "no refresh token available")
  databraryr:::clear_token_bundle()
})

