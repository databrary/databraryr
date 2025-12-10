# get_category_by_id() ---------------------------------------------------
login_test_account()

test_that("get_category_by_id retrieves valid category", {
  # Test with a known category ID (assuming ID 1 exists in test environment)
  result <- get_category_by_id(category_id = 1)
  skip_if_null_response(result, "get_category_by_id(1)")

  expect_type(result, "list")
  expect_named(result, c("category_id", "category_name", "category_description", "metrics"))
  expect_equal(result$category_id, 1)
  expect_type(result$category_name, "character")
  expect_true(nchar(result$category_name) > 0)
})

test_that("get_category_by_id returns NULL for non-existent category", {
  # Use a very large ID that likely doesn't exist
  result <- get_category_by_id(category_id = 999999, vb = FALSE)
  expect_null(result)
})

test_that("get_category_by_id works with verbose mode", {
  result <- get_category_by_id(category_id = 1, vb = TRUE)
  skip_if_null_response(result, "get_category_by_id(1, vb = TRUE)")

  expect_type(result, "list")
  expect_true(!is.null(result$category_id))
})

test_that("get_category_by_id rejects invalid category_id", {
  # Negative ID
  expect_error(get_category_by_id(category_id = -1))

  # Zero ID
  expect_error(get_category_by_id(category_id = 0))

  # Non-numeric ID
  expect_error(get_category_by_id(category_id = "1"))
  expect_error(get_category_by_id(category_id = TRUE))
  expect_error(get_category_by_id(category_id = list(a = 1)))

  # Multiple values
  expect_error(get_category_by_id(category_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(get_category_by_id(category_id = 1.5))
  expect_error(get_category_by_id(category_id = 2.7))

  # NULL
  expect_error(get_category_by_id(category_id = NULL))

  # NA
  expect_error(get_category_by_id(category_id = NA))
})

test_that("get_category_by_id rejects invalid vb parameter", {
  expect_error(get_category_by_id(category_id = 1, vb = -1))
  expect_error(get_category_by_id(category_id = 1, vb = 3))
  expect_error(get_category_by_id(category_id = 1, vb = "a"))
  expect_error(get_category_by_id(category_id = 1, vb = list(a = 1, b = 2)))
  expect_error(get_category_by_id(category_id = 1, vb = c(TRUE, FALSE)))
  expect_error(get_category_by_id(category_id = 1, vb = NULL))
})

test_that("get_category_by_id rejects invalid rq parameter", {
  expect_error(get_category_by_id(category_id = 1, rq = "a"))
  expect_error(get_category_by_id(category_id = 1, rq = -1))
  expect_error(get_category_by_id(category_id = 1, rq = c(2, 3)))
  expect_error(get_category_by_id(category_id = 1, rq = list(a = 1, b = 2)))
  expect_error(get_category_by_id(category_id = 1, rq = TRUE))
})

test_that("get_category_by_id result structure is consistent", {
  result <- get_category_by_id(category_id = 1)
  skip_if_null_response(result, "get_category_by_id(1)")

  # Check that all expected fields exist
  expect_true(all(c("category_id", "category_name", "category_description", "metrics") %in% names(result)))

  # Check field types
  expect_true(is.numeric(result$category_id) || is.integer(result$category_id))
  expect_true(is.character(result$category_name))
  expect_true(is.character(result$category_description) || is.null(result$category_description))
  expect_true(is.list(result$metrics) || is.null(result$metrics))

  # Check that category_id matches the requested ID
  expect_equal(result$category_id, 1)

  # Check that category_name is not empty
  expect_true(nchar(result$category_name) > 0)
})

test_that("get_category_by_id can retrieve multiple different categories", {
  result1 <- get_category_by_id(category_id = 1, vb = FALSE)
  skip_if_null_response(result1, "get_category_by_id(1)")

  # Try to get another category (if available)
  result2 <- get_category_by_id(category_id = 2, vb = FALSE)

  # If both exist, they should be different
  if (!is.null(result2)) {
    expect_false(identical(result1$category_name, result2$category_name))
    expect_equal(result1$category_id, 1)
    expect_equal(result2$category_id, 2)
  }
})

test_that("get_category_by_id works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- get_category_by_id(category_id = 1, rq = custom_rq)
  skip_if_null_response(result, "get_category_by_id(1, rq = custom_rq)")

  expect_type(result, "list")
  expect_equal(result$category_id, 1)
})

test_that("get_category_by_id handles metrics properly", {
  result <- get_category_by_id(category_id = 1)
  skip_if_null_response(result, "get_category_by_id(1)")

  # If metrics exist, check their structure
  if (!is.null(result$metrics) && length(result$metrics) > 0) {
    expect_type(result$metrics, "list")

    # Check first metric structure
    first_metric <- result$metrics[[1]]
    expected_fields <- c("metric_id", "metric_name", "metric_type", "metric_release",
                        "metric_options", "metric_assumed", "metric_description", "metric_required")
    expect_true(all(expected_fields %in% names(first_metric)))

    # Check that metric_id and metric_name are not NULL
    expect_true(!is.null(first_metric$metric_id))
    expect_true(!is.null(first_metric$metric_name))
    expect_true(!is.null(first_metric$metric_type))
  }
})

test_that("get_category_by_id returns expected structure with all fields", {
  result <- get_category_by_id(category_id = 1)
  skip_if_null_response(result, "get_category_by_id(1)")

  # Category should have id, name, description, and metrics fields
  expect_length(result, 4)
  expect_named(result, c("category_id", "category_name", "category_description", "metrics"))
})