# delete_record_measure() ------------------------------------------------------
login_test_account()

test_that("delete_record_measure deletes a measure", {
  record_id <- make_test_record("Delete measure test")
  skip_if_null_response(record_id, "create_volume_record for delete_measure test")

  set_record_measure(vol_id = TEST_VOL_ID, record_id = record_id, metric_id = 30, value = "temp", vb = FALSE)

  delete_result <- delete_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = 30,
    vb = FALSE
  )

  expect_true(delete_result)
})

test_that("delete_record_measure returns FALSE for non-existent measure", {
  record_id <- make_test_record("Delete measure fail test")
  skip_if_null_response(record_id, "create_volume_record for delete_measure fail test")

  delete_result <- delete_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = 29,
    vb = FALSE
  )

  expect_false(delete_result)
})

test_that("delete_record_measure works with verbose mode", {
  record_id <- make_test_record("Delete verbose test")
  skip_if_null_response(record_id, "create_volume_record for delete_measure verbose test")
  set_record_measure(vol_id = TEST_VOL_ID, record_id = record_id, metric_id = 30, value = "temp", vb = FALSE)

  delete_result <- delete_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = 30,
    vb = TRUE
  )

  expect_true(delete_result)
})

test_that("delete_record_measure rejects invalid vol_id", {
  expect_error(delete_record_measure(vol_id = -1, record_id = 1, metric_id = 29))
  expect_error(delete_record_measure(vol_id = 0, record_id = 1, metric_id = 29))
  expect_error(delete_record_measure(vol_id = "1", record_id = 1, metric_id = 29))
  expect_error(delete_record_measure(vol_id = TRUE, record_id = 1, metric_id = 29))
  expect_error(delete_record_measure(vol_id = c(1, 2), record_id = 1, metric_id = 29))
  expect_error(delete_record_measure(vol_id = 1777.5, record_id = 1, metric_id = 29))
})

test_that("delete_record_measure rejects invalid record_id", {
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = -1, metric_id = 29))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 0, metric_id = 29))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = "1", metric_id = 29))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = TRUE, metric_id = 29))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = c(1, 2), metric_id = 29))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1.5, metric_id = 29))
})

test_that("delete_record_measure rejects invalid metric_id", {
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = -1))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 0))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = "1"))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = TRUE))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = c(1, 2)))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 1.5))
})

test_that("delete_record_measure rejects invalid vb parameter", {
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, vb = -1))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, vb = 3))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, vb = "a"))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, vb = c(TRUE, FALSE)))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, vb = NULL))
})

test_that("delete_record_measure rejects invalid rq parameter", {
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, rq = "a"))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, rq = -1))
  expect_error(delete_record_measure(vol_id = TEST_VOL_ID, record_id = 1, metric_id = 29, rq = TRUE))
})

test_that("delete_record_measure works with custom request object", {
  record_id <- make_test_record("Delete custom rq test")
  skip_if_null_response(record_id, "create_volume_record for delete_measure custom rq test")
  set_record_measure(vol_id = TEST_VOL_ID, record_id = record_id, metric_id = 30, value = "temp", vb = FALSE)

  custom_rq <- databraryr::make_default_request()
  delete_result <- delete_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = 30,
    rq = custom_rq,
    vb = FALSE
  )

  expect_true(delete_result)
})
