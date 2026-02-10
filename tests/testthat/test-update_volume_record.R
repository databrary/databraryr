# update_volume_record() -------------------------------------------------------
login_test_account()

test_that("update_volume_record updates an existing record", {
  # First create a record by name
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Initial value",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for update test")

  record_id <- create_result$record_id

  # Update the record
  update_result <- update_volume_record(
    vol_id = 1777,
    record_id = record_id,
    measures = list("29" = "Updated value"),
    vb = FALSE
  )

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

  skip_if_null_response(update_result, "update_volume_record")

  expect_type(update_result, "list")
  expect_named(update_result, c("record_id", "record_volume", "record_category_id", "measures", "birthday", "age"))
  expect_equal(update_result$record_id, record_id)
  expect_true(!is.null(update_result$measures))
})

test_that("update_volume_record returns NULL for non-existent record", {
  result <- update_volume_record(
    vol_id = 1777,
    record_id = 999999,
    measures = list("29" = "Test"),
    vb = FALSE
  )
  expect_null(result)
})

test_that("update_volume_record works with verbose mode", {
  # First create a record
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Update verbose test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for update verbose test")

  record_id <- create_result$record_id

  # Update with verbose
  update_result <- update_volume_record(
    vol_id = 1777,
    record_id = record_id,
    measures = list("29" = "Test"),
    vb = TRUE
  )

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

  skip_if_null_response(update_result, "update_volume_record with vb = TRUE")

  expect_type(update_result, "list")
})

test_that("update_volume_record rejects invalid vol_id", {
  expect_error(update_volume_record(vol_id = -1, record_id = 1))
  expect_error(update_volume_record(vol_id = 0, record_id = 1))
  expect_error(update_volume_record(vol_id = "1", record_id = 1))
  expect_error(update_volume_record(vol_id = TRUE, record_id = 1))
  expect_error(update_volume_record(vol_id = c(1, 2), record_id = 1))
  expect_error(update_volume_record(vol_id = 1777.5, record_id = 1))
})

test_that("update_volume_record rejects invalid record_id", {
  expect_error(update_volume_record(vol_id = 1777, record_id = -1))
  expect_error(update_volume_record(vol_id = 1777, record_id = 0))
  expect_error(update_volume_record(vol_id = 1777, record_id = "1"))
  expect_error(update_volume_record(vol_id = 1777, record_id = TRUE))
  expect_error(update_volume_record(vol_id = 1777, record_id = c(1, 2)))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1.5))
})

test_that("update_volume_record rejects invalid measures", {
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, measures = "text"))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, measures = 123))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, measures = TRUE))
})

test_that("update_volume_record rejects invalid vb parameter", {
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, vb = -1))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, vb = 3))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, vb = "a"))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, vb = NULL))
})

test_that("update_volume_record rejects invalid rq parameter", {
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, rq = "a"))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, rq = -1))
  expect_error(update_volume_record(vol_id = 1777, record_id = 1, rq = TRUE))
})
