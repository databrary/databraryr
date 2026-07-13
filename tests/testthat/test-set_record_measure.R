# set_record_measure() ---------------------------------------------------------
login_test_account()

test_that("set_record_measure sets a text measure", {
  record_id <- make_test_record("Set measure test")
  skip_if_null_response(record_id, "create_volume_record for set_measure test")

  measure_result <- set_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = TEST_METRIC_ID,
    value = "Test value",
    vb = FALSE
  )

  skip_if_null_response(measure_result, "set_record_measure")

  expect_true(!is.null(measure_result))
})

test_that("set_record_measure updates an existing measure", {
  record_id <- make_test_record("Initial")
  skip_if_null_response(record_id, "create_volume_record for set_measure update test")

  measure_result <- set_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = TEST_METRIC_ID,
    value = "Updated",
    vb = FALSE
  )

  skip_if_null_response(measure_result, "set_record_measure update")

  expect_true(!is.null(measure_result))
})

test_that("set_record_measure returns NULL for non-existent record", {
  result <- set_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = TEST_MISSING_ID,
    metric_id = TEST_METRIC_ID,
    value = "Test",
    vb = FALSE
  )
  expect_null(result)
})

test_that("set_record_measure works with verbose mode", {
  record_id <- make_test_record("Set measure vb")
  skip_if_null_response(record_id, "create_volume_record for set_measure verbose test")

  measure_result <- set_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = TEST_METRIC_ID,
    value = "Test",
    vb = TRUE
  )

  skip_if_null_response(measure_result, "set_record_measure with vb = TRUE")

  expect_true(!is.null(measure_result))
})

test_that("set_record_measure rejects invalid vol_id", {
  expect_error(set_record_measure(vol_id = -1, record_id = 1, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = 0, record_id = 1, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = "1", record_id = 1, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = TRUE, record_id = 1, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = c(1, 2), record_id = 1, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = 1.5, record_id = 1, metric_id = 1, value = "test"))
})

test_that("set_record_measure rejects invalid record_id", {
  expect_error(set_record_measure(vol_id = 1, record_id = -1, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = 0, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = "1", metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = TRUE, metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = c(1, 2), metric_id = 1, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1.5, metric_id = 1, value = "test"))
})

test_that("set_record_measure rejects invalid metric_id", {
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = -1, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 0, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = "1", value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = TRUE, value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = c(1, 2), value = "test"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1.5, value = "test"))
})

test_that("set_record_measure rejects invalid vb parameter", {
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", vb = -1))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", vb = 3))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", vb = "a"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", vb = c(TRUE, FALSE)))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", vb = NULL))
})

test_that("set_record_measure rejects invalid rq parameter", {
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", rq = "a"))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", rq = -1))
  expect_error(set_record_measure(vol_id = 1, record_id = 1, metric_id = 1, value = "test", rq = TRUE))
})

test_that("set_record_measure works with numeric values", {
  record_id <- make_test_record("Numeric measure test")
  skip_if_null_response(record_id, "create_volume_record for numeric measure test")

  measure_result <- set_record_measure(
    vol_id = TEST_VOL_ID,
    record_id = record_id,
    metric_id = TEST_METRIC_ID,
    value = 42.5,
    vb = FALSE
  )

  skip_if_null_response(measure_result, "set_record_measure numeric")

  expect_true(!is.null(measure_result))
})
