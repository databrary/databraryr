# list_categories -------------------------------------------------------------
login_test_account()

test_that("list_categories returns tibble with categories", {
  result <- list_categories()
  skip_if_null_response(result, "list_categories()")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("category_id", "category_name", "category_description", "metrics") %in% names(result)))
})

test_that("list_categories returns valid category structure", {
  result <- list_categories(vb = FALSE)
  skip_if_null_response(result, "list_categories()")

  # Check column types
  expect_true(is.numeric(result$category_id) || is.integer(result$category_id))
  expect_type(result$category_name, "character")
  expect_type(result$category_description, "character")
  expect_type(result$metrics, "list")

  # Check that category_ids are positive
  expect_true(all(result$category_id > 0))

  # Check that category names are not empty
  expect_true(all(nchar(result$category_name) > 0))
})

test_that("list_categories works with verbose mode", {
  result <- list_categories(vb = TRUE)
  skip_if_null_response(result, "list_categories(vb = TRUE)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_categories rejects invalid vb parameter", {
  expect_error(list_categories(vb = -1))
  expect_error(list_categories(vb = 3))
  expect_error(list_categories(vb = "a"))
  expect_error(list_categories(vb = list(a = 1, b = 2)))
  expect_error(list_categories(vb = c(TRUE, FALSE)))
  expect_error(list_categories(vb = NULL))
})

test_that("list_categories rejects invalid rq parameter", {
  expect_error(list_categories(rq = "a"))
  expect_error(list_categories(rq = -1))
  expect_error(list_categories(rq = c(2, 3)))
  expect_error(list_categories(rq = list(a = 1, b = 2)))
  expect_error(list_categories(rq = TRUE))
})

test_that("list_categories handles metrics correctly", {
  result <- list_categories()
  skip_if_null_response(result, "list_categories()")

  # Check that metrics column exists and is a list
  expect_true("metrics" %in% names(result))
  expect_type(result$metrics, "list")

  # If any category has metrics, check their structure
  has_metrics <- sapply(result$metrics, function(m) !is.null(m) && length(m) > 0)
  if (any(has_metrics)) {
    # Get first category with metrics
    first_with_metrics <- which(has_metrics)[1]
    metrics <- result$metrics[[first_with_metrics]]

    expect_type(metrics, "list")
    expect_gt(length(metrics), 0)

    # Check first metric structure
    first_metric <- metrics[[1]]
    expected_fields <- c("metric_id", "metric_name", "metric_type", "metric_release",
                        "metric_options", "metric_assumed", "metric_description", "metric_required")
    expect_true(all(expected_fields %in% names(first_metric)))
    expect_true(!is.null(first_metric$metric_id))
    expect_true(!is.null(first_metric$metric_name))
    expect_true(!is.null(first_metric$metric_type))
  }
})

test_that("list_categories works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- list_categories(rq = custom_rq)
  skip_if_null_response(result, "list_categories(rq = custom_rq)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_categories returns consistent number of rows across calls", {
  result1 <- list_categories(vb = FALSE)
  skip_if_null_response(result1, "list_categories() first call")

  result2 <- list_categories(vb = FALSE)
  skip_if_null_response(result2, "list_categories() second call")

  # Number of categories should be stable
  expect_equal(nrow(result1), nrow(result2))
})

test_that("list_categories has unique category IDs", {
  result <- list_categories()
  skip_if_null_response(result, "list_categories()")

  # All category IDs should be unique
  expect_equal(length(unique(result$category_id)), nrow(result))
})
