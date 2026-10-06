# create_volume_record() -------------------------------------------------------
login_test_account()

test_that("create_volume_record creates a record with valid parameters", {
  # Create a record with name (resolves required name metric from volume config)
  result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = "Test condition",
    vb = FALSE
  )
  skip_if_null_response(result, "create_volume_record(vol_id = TEST_VOL_ID, category_id = TEST_CATEGORY_ID, name = 'Test condition')")

  on.exit(delete_volume_record(vol_id = TEST_VOL_ID, record_id = result$record_id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_named(result, c(
    "record_id", "record_volume", "record_volume_name", "record_category_id",
    "measures", "birthday", "age", "default_sessions", "record_source_kind"
  ))
  expect_equal(as.integer(result$record_volume), as.integer(TEST_VOL_ID))
  expect_equal(result$record_category_id, TEST_CATEGORY_ID)
  expect_true(is.numeric(result$record_id) || is.integer(result$record_id))
  expect_true(result$record_id > 0)
})

test_that("create_volume_record creates a record with measures", {
  # Create a record by name (includes required name metric)
  result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = "Test condition",
    vb = FALSE
  )
  skip_if_null_response(result, "create_volume_record with name")

  on.exit(delete_volume_record(vol_id = TEST_VOL_ID, record_id = result$record_id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_true(!is.null(result$measures))
  expect_true(is.list(result$measures))
})

test_that("create_volume_record creates a record with name and additional measures", {
  # Optional metric TEST_METRIC_ID_EXTRA on TEST_CATEGORY_ID in TEST_VOL_ID (see create_volume_record tests).
  result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = "Task with extra measures",
    measures = stats::setNames(list("Extra value"), as.character(TEST_METRIC_ID_EXTRA)),
    vb = FALSE
  )
  skip_if_null_response(result, "create_volume_record with name and measures")

  on.exit(delete_volume_record(vol_id = TEST_VOL_ID, record_id = result$record_id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_true(!is.null(result$measures))
  expect_true(length(result$measures) >= 2L)
})

test_that("create_volume_record rejects invalid name", {
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = ""))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "   "))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = 123))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = c("A", "B")))
})

test_that("create_volume_record returns NULL for non-existent volume", {
  result <- create_volume_record(
    vol_id = TEST_MISSING_ID,
    category_id = TEST_CATEGORY_ID,
    name = "Test",
    vb = FALSE
  )
  expect_null(result)
})

test_that("create_volume_record works with verbose mode", {
  result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = "Test condition vb",
    vb = TRUE
  )
  skip_if_null_response(result, "create_volume_record with vb = TRUE")

  on.exit(delete_volume_record(vol_id = TEST_VOL_ID, record_id = result$record_id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_true(!is.null(result$record_id))
})

test_that("create_volume_record rejects invalid vol_id", {
  # Negative ID
  expect_error(create_volume_record(vol_id = -1, category_id = 1, name = "Test"))

  # Zero ID
  expect_error(create_volume_record(vol_id = 0, category_id = 1, name = "Test"))

  # Non-numeric ID
  expect_error(create_volume_record(vol_id = "1", category_id = 1, name = "Test"))
  expect_error(create_volume_record(vol_id = TRUE, category_id = 1, name = "Test"))
  expect_error(create_volume_record(vol_id = list(a = 1), category_id = 1, name = "Test"))

  # Multiple values
  expect_error(create_volume_record(vol_id = c(1, 2), category_id = 1, name = "Test"))

  # Decimal/non-integer
  expect_error(create_volume_record(vol_id = 1.5, category_id = 1, name = "Test"))
})

test_that("create_volume_record rejects invalid category_id", {
  # Negative ID
  expect_error(create_volume_record(vol_id = 1, category_id = -1, name = "Test"))

  # Zero ID
  expect_error(create_volume_record(vol_id = 1, category_id = 0, name = "Test"))

  # Non-numeric ID
  expect_error(create_volume_record(vol_id = 1, category_id = "1", name = "Test"))
  expect_error(create_volume_record(vol_id = 1, category_id = TRUE, name = "Test"))

  # Multiple values
  expect_error(create_volume_record(vol_id = 1, category_id = c(1, 2), name = "Test"))

  # Decimal/non-integer
  expect_error(create_volume_record(vol_id = 1, category_id = 1.5, name = "Test"))
})

test_that("create_volume_record rejects invalid measures", {
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", measures = "text"))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", measures = 123))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", measures = TRUE))
})

test_that("create_volume_record creates participant with birthdate measure", {
  uniq <- sprintf(
    "create_cr_bday_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )

  result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = 1L,
    name = uniq,
    measures = list("4" = list(year = 2020, month = 3, day = 15)),
    vb = FALSE
  )
  skip_if_null_response(result, "create_volume_record with birthdate measure")

  on.exit(delete_volume_record(vol_id = TEST_VOL_ID, record_id = result$record_id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_true(!is.null(result$measures))
  expect_true("4" %in% names(result$measures))
})

test_that("create_volume_record rejects invalid vb parameter", {
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", vb = -1))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", vb = 3))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", vb = "a"))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", vb = list(a = 1)))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", vb = c(TRUE, FALSE)))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", vb = NULL))
})

test_that("create_volume_record rejects invalid rq parameter", {
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", rq = "a"))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", rq = -1))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", rq = c(2, 3)))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", rq = list(a = 1)))
  expect_error(create_volume_record(vol_id = 1, category_id = 1, name = "Test", rq = TRUE))
})

test_that("create_volume_record works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = "Test condition rq",
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "create_volume_record with custom_rq")

  on.exit(delete_volume_record(vol_id = TEST_VOL_ID, record_id = result$record_id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_true(!is.null(result$record_id))
})
