# make_default_request ---------------------------------------------------------
test_that("make_default_request returns httr2_request", {
  expect_true("httr2_request" %in% class(make_default_request()))
})

test_that("make_default_request optionally attaches bearer token", {
  clear_token_bundle()
  set_token_bundle(access_token = "xyz", refresh_token = NULL)
  req <- make_default_request(with_token = TRUE, refresh = FALSE, vb = FALSE)
  headers <- req$headers
  expect_equal(headers$Authorization, "Bearer xyz")
  clear_token_bundle()
})

test_that("make_default_request errors when token missing", {
  clear_token_bundle()
  expect_error(make_default_request(with_token = TRUE, refresh = FALSE, vb = FALSE),
               "No OAuth token available")
})

