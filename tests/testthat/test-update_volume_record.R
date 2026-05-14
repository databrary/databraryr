# update_volume_record() -------------------------------------------------------
login_test_account()

test_that("update_volume_record updates an existing record", {
  record_id <- make_test_record("Initial value")
  skip_if_null_response(record_id, "create_volume_record for update test")

  update_result <- update_volume_record(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    measures = list("29" = "Updated value"),
    vb = FALSE
  )

  skip_if_null_response(update_result, "update_volume_record")

  expect_type(update_result, "list")
  expect_named(update_result, c(
    "record_id", "record_volume", "record_volume_name", "record_category_id",
    "measures", "birthday", "age", "default_sessions", "record_source_kind"
  ))
  expect_equal(update_result$record_id, record_id)
  expect_true(!is.null(update_result$measures))
})

test_that("update_volume_record returns NULL for non-existent record", {
  result <- update_volume_record(
    vol_id = TEST_VOL_ID,
    record_id = 999999,
    measures = list("29" = "Test"),
    vb = FALSE
  )
  expect_null(result)
})

test_that("update_volume_record works with verbose mode", {
  record_id <- make_test_record("Update verbose test")
  skip_if_null_response(record_id, "create_volume_record for update verbose test")

  update_result <- update_volume_record(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    measures = list("29" = "Test"),
    vb = TRUE
  )

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
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = -1))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 0))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = "1"))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = TRUE))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = c(1, 2)))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1.5))
})

test_that("update_volume_record rejects invalid measures", {
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, measures = "text"))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, measures = 123))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, measures = TRUE))
})

test_that("update_volume_record rejects invalid vb parameter", {
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, vb = -1))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, vb = 3))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, vb = "a"))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, vb = NULL))
})

test_that("update_volume_record rejects invalid rq parameter", {
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, rq = "a"))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, rq = -1))
  expect_error(update_volume_record(vol_id = TEST_VOL_ID, record_id = 1, rq = TRUE))
})
