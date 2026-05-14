# get_volume_record_by_id() --------------------------------------------------
login_test_account()

records_vol_1777 <- list_volume_records(vol_id = 1777, vb = FALSE)

test_that("get_volume_record_by_id retrieves valid record", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  if (nrow(records) > 0) {
    test_record_id <- records$record_id[1]

    result <- get_volume_record_by_id(vol_id = 1777, record_id = test_record_id)
    skip_if_null_response(result, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d)", test_record_id))

    expect_type(result, "list")
    expect_named(result, c(
      "record_id", "record_volume", "record_volume_name", "record_category_id",
      "measures", "birthday", "age", "default_sessions", "record_source_kind"
    ))
    expect_equal(result$record_id, test_record_id)
    # record_volume is the owning volume id; linked records may differ from requested vol_id
    row <- match(test_record_id, records$record_id)
    expect_equal(result$record_volume, records$record_volume[row])
    expect_true(is.numeric(result$record_category_id) || is.integer(result$record_category_id))
  }
})

test_that("get_volume_record_by_id returns NULL for non-existent record", {
  # Use a very large ID that likely doesn't exist
  result <- get_volume_record_by_id(vol_id = 1777, record_id = 999999, vb = FALSE)
  expect_null(result)
})

test_that("get_volume_record_by_id returns NULL for non-existent volume", {
  result <- get_volume_record_by_id(vol_id = 999999, record_id = 1, vb = FALSE)
  expect_null(result)
})

test_that("get_volume_record_by_id works with verbose mode", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  if (nrow(records) > 0) {
    test_record_id <- records$record_id[1]
    result <- get_volume_record_by_id(vol_id = 1777, record_id = test_record_id, vb = TRUE)
    skip_if_null_response(result, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d, vb = TRUE)", test_record_id))

    expect_type(result, "list")
    expect_true(!is.null(result$record_id))
  }
})

test_that("get_volume_record_by_id rejects invalid vol_id", {
  # Negative ID
  expect_error(get_volume_record_by_id(vol_id = -1, record_id = 1))

  # Zero ID
  expect_error(get_volume_record_by_id(vol_id = 0, record_id = 1))

  # Non-numeric ID
  expect_error(get_volume_record_by_id(vol_id = "1", record_id = 1))
  expect_error(get_volume_record_by_id(vol_id = TRUE, record_id = 1))
  expect_error(get_volume_record_by_id(vol_id = list(a = 1), record_id = 1))

  # Multiple values
  expect_error(get_volume_record_by_id(vol_id = c(1, 2), record_id = 1))

  # Decimal/non-integer
  expect_error(get_volume_record_by_id(vol_id = 1777.5, record_id = 1))
  expect_error(get_volume_record_by_id(vol_id = 2.7, record_id = 1))

  # NULL
  expect_error(get_volume_record_by_id(vol_id = NULL, record_id = 1))

  # NA
  expect_error(get_volume_record_by_id(vol_id = NA, record_id = 1))
})

test_that("get_volume_record_by_id rejects invalid record_id", {
  # Negative ID
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = -1))

  # Zero ID
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 0))

  # Non-numeric ID
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = "1"))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = TRUE))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = list(a = 1)))

  # Multiple values
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1.5))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 2.7))

  # NULL
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = NULL))

  # NA
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = NA))
})

test_that("get_volume_record_by_id rejects invalid vb parameter", {
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, vb = -1))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, vb = 3))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, vb = "a"))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, vb = list(a = 1, b = 2)))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, vb = NULL))
})

test_that("get_volume_record_by_id rejects invalid rq parameter", {
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, rq = "a"))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, rq = -1))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, rq = c(2, 3)))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, rq = list(a = 1, b = 2)))
  expect_error(get_volume_record_by_id(vol_id = 1777, record_id = 1, rq = TRUE))
})

test_that("get_volume_record_by_id result structure is consistent", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  if (nrow(records) > 0) {
    test_record_id <- records$record_id[1]
    result <- get_volume_record_by_id(vol_id = 1777, record_id = test_record_id)
    skip_if_null_response(result, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d)", test_record_id))

    # Check that all expected fields exist
    expect_true(all(c(
      "record_id", "record_volume", "record_volume_name", "record_category_id",
      "measures", "birthday", "age", "default_sessions", "record_source_kind"
    ) %in% names(result)))

    # Check field types
    expect_true(is.numeric(result$record_id) || is.integer(result$record_id))
    expect_true(is.numeric(result$record_volume) || is.integer(result$record_volume))
    expect_true(is.character(result$record_volume_name))
    expect_true(is.numeric(result$record_category_id) || is.integer(result$record_category_id))
    expect_true(is.list(result$measures) || is.null(result$measures))
    expect_true(is.list(result$default_sessions))

    # Check that record_id matches the requested ID
    expect_equal(result$record_id, test_record_id)

    # Owning volume may differ from requested vol_id when the list includes linked records
    row <- match(test_record_id, records$record_id)
    expect_equal(result$record_volume, records$record_volume[row])
  }
})

test_that("get_volume_record_by_id handles age structure correctly", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  if (nrow(records) == 0) {
    testthat::skip("No records on volume; cannot validate age structure")
  }

  test_record_id <- records$record_id[1]
  result <- get_volume_record_by_id(vol_id = 1777, record_id = test_record_id)
  skip_if_null_response(result, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d)", test_record_id))

  if (is.null(result$age)) {
    expect_null(result$age)
  } else {
    expect_type(result$age, "list")
    expected_fields <- c("years", "months", "days", "total_days", "formatted_value", "is_estimated", "is_blurred")
    expect_true(all(expected_fields %in% names(result$age)))
  }
})

test_that("get_volume_record_by_id handles measures correctly", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  if (nrow(records) > 0) {
    test_record_id <- records$record_id[1]
    result <- get_volume_record_by_id(vol_id = 1777, record_id = test_record_id)
    skip_if_null_response(result, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d)", test_record_id))

    # Measures should be a list (can be empty)
    expect_true(is.list(result$measures) || is.null(result$measures))
  }
})

test_that("get_volume_record_by_id works with custom request object", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  if (nrow(records) > 0) {
    test_record_id <- records$record_id[1]
    custom_rq <- databraryr::make_default_request()
    result <- get_volume_record_by_id(vol_id = 1777, record_id = test_record_id, rq = custom_rq)
    skip_if_null_response(result, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d, rq = custom_rq)", test_record_id))

    expect_type(result, "list")
    expect_equal(result$record_id, test_record_id)
  }
})

test_that("get_volume_record_by_id can retrieve multiple different records", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  unique_ids <- unique(records$record_id)
  if (length(unique_ids) < 2) {
    testthat::skip("Volume has fewer than 2 records; need 2+ to verify multiple retrieval")
  }

  record_id_1 <- unique_ids[1]
  record_id_2 <- unique_ids[2]

  result1 <- get_volume_record_by_id(vol_id = 1777, record_id = record_id_1, vb = FALSE)
  result2 <- get_volume_record_by_id(vol_id = 1777, record_id = record_id_2, vb = FALSE)

  skip_if_null_response(result1, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d)", record_id_1))
  skip_if_null_response(result2, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d)", record_id_2))

  expect_false(identical(result1$record_id, result2$record_id))
  expect_equal(as.integer(result1$record_id), as.integer(record_id_1))
  expect_equal(as.integer(result2$record_id), as.integer(record_id_2))
})

test_that("get_volume_record_by_id returns complete structure with all fields", {
  records <- records_vol_1777
  skip_if_null_response(records, "list_volume_records(vol_id = 1777)")

  if (nrow(records) > 0) {
    test_record_id <- records$record_id[1]
    result <- get_volume_record_by_id(vol_id = 1777, record_id = test_record_id)
    skip_if_null_response(result, sprintf("get_volume_record_by_id(vol_id = 1777, record_id = %d)", test_record_id))

    # Record should have all expected fields (parity with RecordSerializer read-only output)
    expect_length(result, 9)
    expect_named(result, c(
      "record_id", "record_volume", "record_volume_name", "record_category_id",
      "measures", "birthday", "age", "default_sessions", "record_source_kind"
    ))
  }
})
