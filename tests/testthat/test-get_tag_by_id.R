# get_tag_by_id() ---------------------------------------------------
login_test_account()

test_that("get_tag_by_id retrieves valid tag", {
  # Test with a known tag ID (assuming ID 1 exists in test environment)
  result <- get_tag_by_id(tag_id = 1)
  skip_if_null_response(result, "get_tag_by_id(1)")

  expect_type(result, "list")
  expect_named(result, c("tag_id", "tag_name"))
  expect_equal(result$tag_id, 1)
  expect_type(result$tag_name, "character")
  expect_true(nchar(result$tag_name) > 0)
})

test_that("get_tag_by_id returns NULL for non-existent tag", {
  # Use a very large ID that likely doesn't exist
  result <- get_tag_by_id(tag_id = 999999, vb = FALSE)
  expect_null(result)
})

test_that("get_tag_by_id works with verbose mode", {
  result <- get_tag_by_id(tag_id = 1, vb = TRUE)
  skip_if_null_response(result, "get_tag_by_id(1, vb = TRUE)")

  expect_type(result, "list")
  expect_true(!is.null(result$tag_id))
})

test_that("get_tag_by_id rejects invalid tag_id", {
  # Negative ID
  expect_error(get_tag_by_id(tag_id = -1))

  # Zero ID
  expect_error(get_tag_by_id(tag_id = 0))

  # Non-numeric ID
  expect_error(get_tag_by_id(tag_id = "1"))
  expect_error(get_tag_by_id(tag_id = TRUE))
  expect_error(get_tag_by_id(tag_id = list(a = 1)))

  # Multiple values
  expect_error(get_tag_by_id(tag_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(get_tag_by_id(tag_id = 1.5))
  expect_error(get_tag_by_id(tag_id = 2.7))

  # NULL
  expect_error(get_tag_by_id(tag_id = NULL))

  # NA
  expect_error(get_tag_by_id(tag_id = NA))
})

test_that("get_tag_by_id rejects invalid vb parameter", {
  expect_error(get_tag_by_id(tag_id = 1, vb = -1))
  expect_error(get_tag_by_id(tag_id = 1, vb = 3))
  expect_error(get_tag_by_id(tag_id = 1, vb = "a"))
  expect_error(get_tag_by_id(tag_id = 1, vb = list(a = 1, b = 2)))
  expect_error(get_tag_by_id(tag_id = 1, vb = c(TRUE, FALSE)))
  expect_error(get_tag_by_id(tag_id = 1, vb = NULL))
})

test_that("get_tag_by_id rejects invalid rq parameter", {
  expect_error(get_tag_by_id(tag_id = 1, rq = "a"))
  expect_error(get_tag_by_id(tag_id = 1, rq = -1))
  expect_error(get_tag_by_id(tag_id = 1, rq = c(2, 3)))
  expect_error(get_tag_by_id(tag_id = 1, rq = list(a = 1, b = 2)))
  expect_error(get_tag_by_id(tag_id = 1, rq = TRUE))
})

test_that("get_tag_by_id result structure is consistent", {
  result <- get_tag_by_id(tag_id = 1)
  skip_if_null_response(result, "get_tag_by_id(1)")

  # Check that all expected fields exist
  expect_true(all(c("tag_id", "tag_name") %in% names(result)))

  # Check field types
  expect_true(is.numeric(result$tag_id) || is.integer(result$tag_id))
  expect_true(is.character(result$tag_name))

  # Check that tag_id matches the requested ID
  expect_equal(result$tag_id, 1)

  # Check that tag_name is not empty
  expect_true(nchar(result$tag_name) > 0)
})

test_that("get_tag_by_id can retrieve multiple different tags", {
  result1 <- get_tag_by_id(tag_id = 1, vb = FALSE)
  skip_if_null_response(result1, "get_tag_by_id(1)")

  # Try to get another tag (if available)
  result2 <- get_tag_by_id(tag_id = 2, vb = FALSE)

  # If both exist, they should be different
  if (!is.null(result2)) {
    expect_false(identical(result1$tag_name, result2$tag_name))
    expect_equal(result1$tag_id, 1)
    expect_equal(result2$tag_id, 2)
  }
})

test_that("get_tag_by_id works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- get_tag_by_id(tag_id = 1, rq = custom_rq)
  skip_if_null_response(result, "get_tag_by_id(1, rq = custom_rq)")

  expect_type(result, "list")
  expect_equal(result$tag_id, 1)
})

test_that("get_tag_by_id returns simple structure with only id and name", {
  result <- get_tag_by_id(tag_id = 1)
  skip_if_null_response(result, "get_tag_by_id(1)")

  # Tag should only have id and name fields
  expect_length(result, 2)
  expect_named(result, c("tag_id", "tag_name"))
})
