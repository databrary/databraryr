test_that("whoami returns NULL when unauthenticated", {
  clear_token_bundle()
  expect_null(whoami(refresh = FALSE, vb = FALSE))
})

test_that("whoami fetches user info", {
  clear_token_bundle()
  set_token_bundle(access_token = "abc", refresh_token = NULL)

  local_mocked_bindings(
    req_perform = function(...) {
      httr2::response(
        method = "GET",
        url = OAUTH_TEST_URL,
        status_code = 200,
        headers = list("Content-Type" = "application/json"),
        body = charToRaw('{"auth_method":"password","user":{"id":1}}')
      )
    },
    .package = "httr2"
  )

  result <- whoami(refresh = FALSE, vb = FALSE)

  expect_equal(result$auth_method, "password")
  expect_equal(result$user$id, 1)
  clear_token_bundle()
})

