# delete_record_measure() ------------------------------------------------------
login_test_account()

test_that("delete_record_measure deletes a measure", {
  # Create a record and add an optional measure (metric 29 is name/required)
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Delete measure test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for delete_measure test")

  record_id <- create_result$record_id

  # First set an optional measure (30), then delete it
  set_record_measure(vol_id = 1777, record_id = record_id, metric_id = 30, value = "temp", vb = FALSE)

  # Delete the non-required measure
  delete_result <- delete_record_measure(
    vol_id = 1777,
    record_id = record_id,
    metric_id = 30,
    vb = FALSE
  )

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

  expect_true(delete_result)
})

test_that("delete_record_measure returns FALSE for non-existent measure", {
  # Create a record (name is the required measure)
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Delete measure fail test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for delete_measure fail test")

  record_id <- create_result$record_id

  # Try to delete a non-existent measure
  delete_result <- delete_record_measure(
    vol_id = 1777,
    record_id = record_id,
    metric_id = 29,
    vb = FALSE
  )

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

  expect_false(delete_result)
})

test_that("delete_record_measure works with verbose mode", {
  # Create a record and add optional measure
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Delete verbose test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for delete_measure verbose test")

  record_id <- create_result$record_id
  set_record_measure(vol_id = 1777, record_id = record_id, metric_id = 30, value = "temp", vb = FALSE)

  # Delete with verbose
  delete_result <- delete_record_measure(
    vol_id = 1777,
    record_id = record_id,
    metric_id = 30,
    vb = TRUE
  )

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

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
  expect_error(delete_record_measure(vol_id = 1777, record_id = -1, metric_id = 29))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 0, metric_id = 29))
  expect_error(delete_record_measure(vol_id = 1777, record_id = "1", metric_id = 29))
  expect_error(delete_record_measure(vol_id = 1777, record_id = TRUE, metric_id = 29))
  expect_error(delete_record_measure(vol_id = 1777, record_id = c(1, 2), metric_id = 29))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1.5, metric_id = 29))
})

test_that("delete_record_measure rejects invalid metric_id", {
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = -1))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 0))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = "1"))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = TRUE))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = c(1, 2)))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 1.5))
})

test_that("delete_record_measure rejects invalid vb parameter", {
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, vb = -1))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, vb = 3))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, vb = "a"))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, vb = c(TRUE, FALSE)))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, vb = NULL))
})

test_that("delete_record_measure rejects invalid rq parameter", {
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, rq = "a"))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, rq = -1))
  expect_error(delete_record_measure(vol_id = 1777, record_id = 1, metric_id = 29, rq = TRUE))
})

test_that("delete_record_measure works with custom request object", {
  # Create a record and add optional measure
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = "Delete custom rq test",
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for delete_measure custom rq test")

  record_id <- create_result$record_id
  set_record_measure(vol_id = 1777, record_id = record_id, metric_id = 30, value = "temp", vb = FALSE)

  # Delete with custom request
  custom_rq <- databraryr::make_default_request()
  delete_result <- delete_record_measure(
    vol_id = 1777,
    record_id = record_id,
    metric_id = 30,
    rq = custom_rq,
    vb = FALSE
  )

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

  expect_true(delete_result)
})
