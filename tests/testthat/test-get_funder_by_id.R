# get_funder_by_id() ---------------------------------------------------
login_test_account()

test_that("get_funder_by_id retrieves valid funder", {
  # Test with a known funder ID (assuming ID 1 exists in test environment)
  result <- get_funder_by_id(funder_id = 1)
  skip_if_null_response(result, "get_funder_by_id(1)")

  expect_type(result, "list")
  expect_named(result, c("funder_id", "funder_name", "funder_is_approved"))
  expect_equal(result$funder_id, 1)
  expect_type(result$funder_name, "character")
  expect_type(result$funder_is_approved, "logical")
})

test_that("get_funder_by_id returns NULL for non-existent funder", {
  # Use a very large ID that likely doesn't exist
  result <- get_funder_by_id(funder_id = TEST_MISSING_ID, vb = FALSE)
  expect_null(result)
})

test_that("get_funder_by_id works with verbose mode", {
  result <- get_funder_by_id(funder_id = 1, vb = TRUE)
  skip_if_null_response(result, "get_funder_by_id(1, vb = TRUE)")

  expect_type(result, "list")
  expect_true(!is.null(result$funder_id))
})

test_that("get_funder_by_id rejects invalid funder_id", {
  # Negative ID
  expect_error(get_funder_by_id(funder_id = -1))

  # Zero ID
  expect_error(get_funder_by_id(funder_id = 0))

  # Non-numeric ID
  expect_error(get_funder_by_id(funder_id = "1"))
  expect_error(get_funder_by_id(funder_id = TRUE))
  expect_error(get_funder_by_id(funder_id = list(a = 1)))

  # Multiple values
  expect_error(get_funder_by_id(funder_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(get_funder_by_id(funder_id = 1.5))

  # NULL
  expect_error(get_funder_by_id(funder_id = NULL))

  # NA
  expect_error(get_funder_by_id(funder_id = NA))
})

test_that("get_funder_by_id rejects invalid vb parameter", {
  expect_error(get_funder_by_id(funder_id = 1, vb = -1))
  expect_error(get_funder_by_id(funder_id = 1, vb = 3))
  expect_error(get_funder_by_id(funder_id = 1, vb = "a"))
  expect_error(get_funder_by_id(funder_id = 1, vb = list(a = 1, b = 2)))
  expect_error(get_funder_by_id(funder_id = 1, vb = c(TRUE, FALSE)))
  expect_error(get_funder_by_id(funder_id = 1, vb = NULL))
})

test_that("get_funder_by_id rejects invalid rq parameter", {
  expect_error(get_funder_by_id(funder_id = 1, rq = "a"))
  expect_error(get_funder_by_id(funder_id = 1, rq = -1))
  expect_error(get_funder_by_id(funder_id = 1, rq = c(2, 3)))
  expect_error(get_funder_by_id(funder_id = 1, rq = list(a = 1, b = 2)))
  expect_error(get_funder_by_id(funder_id = 1, rq = TRUE))
})

test_that("get_funder_by_id result structure is consistent", {
  result <- get_funder_by_id(funder_id = 1)
  skip_if_null_response(result, "get_funder_by_id(1)")

  # Check that all expected fields exist
  expect_true(all(c("funder_id", "funder_name", "funder_is_approved") %in% names(result)))

  # Check field types
  expect_true(is.numeric(result$funder_id) || is.integer(result$funder_id))
  expect_true(is.character(result$funder_name))
  expect_true(is.logical(result$funder_is_approved))

  # Check that funder_id matches the requested ID
  expect_equal(result$funder_id, 1)
})

test_that("get_funder_by_id can retrieve multiple different funders", {
  result1 <- get_funder_by_id(funder_id = 1, vb = FALSE)
  skip_if_null_response(result1, "get_funder_by_id(1)")

  # Try to get another funder (if available)
  result2 <- get_funder_by_id(funder_id = 2, vb = FALSE)

  # If both exist, they should be different
  if (!is.null(result2)) {
    expect_false(identical(result1$funder_name, result2$funder_name))
    expect_equal(result1$funder_id, 1)
    expect_equal(result2$funder_id, 2)
  }
})

test_that("get_funder_by_id works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- get_funder_by_id(funder_id = 1, rq = custom_rq)
  skip_if_null_response(result, "get_funder_by_id(1, rq = custom_rq)")

  expect_type(result, "list")
  expect_equal(result$funder_id, 1)
})