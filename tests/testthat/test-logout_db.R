test_that("logout_db rejects bad input parameters", {
  expect_error(logout_db(vb = -1))
  expect_error(logout_db(vb = 3))
  expect_error(logout_db(vb = "a"))
  expect_error(logout_db(vb = c(TRUE, FALSE)))
})

test_that("logout_db clears token state", {
  set_token_bundle(access_token = "abc", refresh_token = "def", expires_in = 3600)
  expect_true(logout_db(vb = FALSE))
  expect_null(get_token_bundle())
})
