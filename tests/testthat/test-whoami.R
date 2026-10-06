test_that("whoami returns NULL when unauthenticated", {
  clear_token_bundle()
  expect_null(whoami(refresh = FALSE, vb = FALSE))
})

test_that("whoami fetches user info", {
  clear_token_bundle()
  login_test_account()
  on.exit(clear_token_bundle(), add = TRUE)

  result <- whoami(refresh = TRUE, vb = FALSE)
  skip_if_null_response(result, "whoami")

  expect_true(nzchar(result$message))
  expect_match(result$path, "oauth2/test")
  expect_equal(result$authMethod, "OAuth2")
})

