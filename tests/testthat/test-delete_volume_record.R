# delete_volume_record() -------------------------------------------------------
login_test_account()

test_that("delete_volume_record deletes an existing record", {
  # First create a record
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Delete test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for delete test")

  record_id <- create_result$record_id

  # Delete the record
  delete_result <- delete_volume_record(
    vol_id = 1777,
    record_id = record_id,
    vb = FALSE
  )

  expect_true(delete_result)

  # Verify it's gone
  get_result <- get_volume_record_by_id(
    vol_id = 1777,
    record_id = record_id,
    vb = FALSE
  )
  expect_null(get_result)
})

test_that("delete_volume_record returns FALSE for non-existent record", {
  result <- delete_volume_record(
    vol_id = 1777,
    record_id = 999999,
    vb = FALSE
  )
  expect_false(result)
})

test_that("delete_volume_record works with verbose mode", {
  # First create a record
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Delete test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for delete verbose test")

  record_id <- create_result$record_id

  # Delete with verbose
  delete_result <- delete_volume_record(
    vol_id = 1777,
    record_id = record_id,
    vb = TRUE
  )

  expect_true(delete_result)
})

test_that("delete_volume_record rejects invalid vol_id", {
  expect_error(delete_volume_record(vol_id = -1, record_id = 1))
  expect_error(delete_volume_record(vol_id = 0, record_id = 1))
  expect_error(delete_volume_record(vol_id = "1", record_id = 1))
  expect_error(delete_volume_record(vol_id = TRUE, record_id = 1))
  expect_error(delete_volume_record(vol_id = list(a = 1), record_id = 1))
  expect_error(delete_volume_record(vol_id = c(1, 2), record_id = 1))
  expect_error(delete_volume_record(vol_id = 1777.5, record_id = 1))
  expect_error(delete_volume_record(vol_id = NULL, record_id = 1))
  expect_error(delete_volume_record(vol_id = NA, record_id = 1))
})

test_that("delete_volume_record rejects invalid record_id", {
  expect_error(delete_volume_record(vol_id = 1777, record_id = -1))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 0))
  expect_error(delete_volume_record(vol_id = 1777, record_id = "1"))
  expect_error(delete_volume_record(vol_id = 1777, record_id = TRUE))
  expect_error(delete_volume_record(vol_id = 1777, record_id = list(a = 1)))
  expect_error(delete_volume_record(vol_id = 1777, record_id = c(1, 2)))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1.5))
  expect_error(delete_volume_record(vol_id = 1777, record_id = NULL))
  expect_error(delete_volume_record(vol_id = 1777, record_id = NA))
})

test_that("delete_volume_record rejects invalid vb parameter", {
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, vb = -1))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, vb = 3))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, vb = "a"))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, vb = list(a = 1, b = 2)))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, vb = NULL))
})

test_that("delete_volume_record rejects invalid rq parameter", {
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, rq = "a"))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, rq = -1))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, rq = c(2, 3)))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, rq = list(a = 1, b = 2)))
  expect_error(delete_volume_record(vol_id = 1777, record_id = 1, rq = TRUE))
})

test_that("delete_volume_record works with custom request object", {
  # First create a record
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Delete custom rq test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for delete custom rq test")

  record_id <- create_result$record_id

  # Delete with custom request
  custom_rq <- databraryr::make_default_request()
  delete_result <- delete_volume_record(
    vol_id = 1777,
    record_id = record_id,
    rq = custom_rq,
    vb = FALSE
  )

  expect_true(delete_result)
})
