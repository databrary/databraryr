# list_volume_records ---------------------------------------------------------
login_test_account()

records_1777 <- list_volume_records(vol_id = 1777, vb = FALSE)

test_that("list_volume_records returns tibble given valid vol_id", {
  result <- records_1777
  skip_if_null_response(result, "list_volume_records(vol_id = 1777)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c(
    "record_id", "record_volume", "record_volume_name", "record_category_id",
    "record_measures", "record_default_sessions", "record_source_kind"
  ) %in% names(result)))
})

test_that("list_volume_records returns valid record structure", {
  result <- records_1777
  skip_if_null_response(result, "list_volume_records(vol_id = 1777)")

  # Check column types
  expect_true(is.numeric(result$record_id) || is.integer(result$record_id))
  expect_true(is.numeric(result$record_volume) || is.integer(result$record_volume))
  expect_true(is.numeric(result$record_category_id) || is.integer(result$record_category_id))
  expect_type(result$record_measures, "list")

  # Check that record_ids are positive
  expect_true(all(result$record_id > 0))

  # record_volume_name / linked provenance / default sessions (core RecordSerializer)
  expect_true(is.character(result$record_volume_name))
  expect_true(is.list(result$record_default_sessions))
  expect_true(is.character(result$record_source_kind))
})

test_that("list_volume_records returns NULL for non-existent volume", {
  result <- list_volume_records(vol_id = 999999, vb = FALSE)
  expect_null(result)
})

test_that("list_volume_records works with category_id filter", {
  all_records <- records_1777
  skip_if_null_response(all_records, "list_volume_records(vol_id = 1777)")

  if (nrow(all_records) > 0) {
    # Get unique category_id from results
    test_category <- all_records$record_category_id[1]

    # Filter by that category
    filtered_records <- list_volume_records(vol_id = 1777, category_id = test_category, vb = FALSE)
    skip_if_null_response(filtered_records, sprintf("list_volume_records(vol_id = 1777, category_id = %d)", test_category))

    # All records should have the specified category_id
    expect_true(all(filtered_records$record_category_id == test_category))
  }
})

test_that("list_volume_records works with verbose mode", {
  result <- list_volume_records(vol_id = 1777, vb = TRUE)
  skip_if_null_response(result, "list_volume_records(vol_id = 1777, vb = TRUE)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_volume_records rejects invalid vol_id", {
  # Negative ID
  expect_error(list_volume_records(vol_id = -1))

  # Zero ID
  expect_error(list_volume_records(vol_id = 0))

  # Non-numeric ID
  expect_error(list_volume_records(vol_id = "1"))
  expect_error(list_volume_records(vol_id = TRUE))
  expect_error(list_volume_records(vol_id = list(a = 1)))

  # Multiple values
  expect_error(list_volume_records(vol_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(list_volume_records(vol_id = 1777.5))
})

test_that("list_volume_records rejects invalid category_id", {
  # Negative ID
  expect_error(list_volume_records(vol_id = 1777, category_id = -1))

  # Zero ID
  expect_error(list_volume_records(vol_id = 1777, category_id = 0))

  # Non-numeric ID
  expect_error(list_volume_records(vol_id = 1777, category_id = "1"))
  expect_error(list_volume_records(vol_id = 1777, category_id = TRUE))

  # Multiple values
  expect_error(list_volume_records(vol_id = 1777, category_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(list_volume_records(vol_id = 1777, category_id = 1.5))
})

test_that("list_volume_records rejects invalid vb parameter", {
  expect_error(list_volume_records(vol_id = 1777, vb = -1))
  expect_error(list_volume_records(vol_id = 1777, vb = 3))
  expect_error(list_volume_records(vol_id = 1777, vb = "a"))
  expect_error(list_volume_records(vol_id = 1777, vb = list(a = 1, b = 2)))
  expect_error(list_volume_records(vol_id = 1777, vb = c(TRUE, FALSE)))
  expect_error(list_volume_records(vol_id = 1777, vb = NULL))
})

test_that("list_volume_records rejects invalid rq parameter", {
  expect_error(list_volume_records(vol_id = 1777, rq = "a"))
  expect_error(list_volume_records(vol_id = 1777, rq = -1))
  expect_error(list_volume_records(vol_id = 1777, rq = c(2, 3)))
  expect_error(list_volume_records(vol_id = 1777, rq = list(a = 1, b = 2)))
  expect_error(list_volume_records(vol_id = 1777, rq = TRUE))
})

test_that("list_volume_records includes age fields", {
  result <- records_1777
  skip_if_null_response(result, "list_volume_records(vol_id = 1777)")

  # Check that age fields exist
  age_fields <- c("age_years", "age_months", "age_days", "age_total_days",
                  "age_formatted", "age_is_estimated", "age_is_blurred")
  expect_true(all(age_fields %in% names(result)))
})

test_that("list_volume_records includes measures as list column", {
  result <- records_1777
  skip_if_null_response(result, "list_volume_records(vol_id = 1777)")

  # Check that measures column is a list
  expect_true("record_measures" %in% names(result))
  expect_type(result$record_measures, "list")
})

test_that("list_volume_records works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- list_volume_records(vol_id = 1777, rq = custom_rq)
  skip_if_null_response(result, "list_volume_records(vol_id = 1777, rq = custom_rq)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_volume_records returns for different volumes", {
  result1 <- records_1777
  skip_if_null_response(result1, "list_volume_records(vol_id = 1777)")

  result2 <- list_volume_records(vol_id = 2, vb = FALSE)
  skip_if_null_response(result2, "list_volume_records(vol_id = 2)")

  # Both should be tibbles
  expect_s3_class(result1, "tbl_df")
  expect_s3_class(result2, "tbl_df")

  # Owning-volume ids may differ from requested vol_id when volumes expose linked records
  expect_true(all(is.finite(result1$record_volume)))
  expect_true(all(is.finite(result2$record_volume)))
  expect_gt(length(unique(result1$record_id)), 0)
  expect_gt(length(unique(result2$record_id)), 0)
})
