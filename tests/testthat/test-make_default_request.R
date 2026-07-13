# make_default_request ---------------------------------------------------------
test_that("make_default_request returns httr2_request after login", {
  login_test_account()
  expect_true("httr2_request" %in% class(make_default_request()))
})

test_that("make_default_request can skip token", {
  req <- make_default_request(with_token = FALSE)
  expect_true("httr2_request" %in% class(req))
  expect_false("Authorization" %in% names(req$headers))
})

