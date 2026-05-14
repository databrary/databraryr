# create_session() -------------------------------------------------------------
login_test_account()

test_that("create_session creates a session with name only", {
  result <- create_session(vol_id = TEST_VOL_ID, name = "Test session", vb = FALSE)
  skip_if_null_response(result, "create_session(vol_id = TEST_VOL_ID, name = 'Test session')")

  on.exit(delete_session(vol_id = TEST_VOL_ID, session_id = result$id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_true(!is.null(result$id))
  expect_true(is.numeric(result$id) || is.integer(result$id))
  expect_true(result$id > 0)
  expect_equal(result$name, "Test session")
  # POST create returns SessionWriteSerializer data (no nested `volume`); confirm volume via GET.
  fetched <- get_session_by_id(session_id = result$id, vol_id = TEST_VOL_ID, vb = FALSE)
  skip_if_null_response(fetched, "get_session_by_id after create_session")
  v <- fetched$volume
  vol_id_actual <- if (is.list(v) && !is.null(v$id)) {
    as.integer(v$id)
  } else {
    as.integer(v)
  }
  expect_equal(vol_id_actual, as.integer(TEST_VOL_ID))
})

test_that("create_session accepts a Date source_date", {
  result <- create_session(
    vol_id = TEST_VOL_ID,
    name = "Test session with Date",
    source_date = as.Date("2024-03-15"),
    vb = FALSE
  )
  skip_if_null_response(result, "create_session with Date source_date")

  on.exit(delete_session(vol_id = TEST_VOL_ID, session_id = result$id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
  expect_true(!is.null(result$id))
})

test_that("create_session accepts an ISO string source_date", {
  result <- create_session(
    vol_id = TEST_VOL_ID,
    name = "Test session with ISO date",
    source_date = "2024-03-15",
    vb = FALSE
  )
  skip_if_null_response(result, "create_session with ISO source_date")

  on.exit(delete_session(vol_id = TEST_VOL_ID, session_id = result$id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
})

test_that("create_session works with verbose mode", {
  result <- create_session(
    vol_id = TEST_VOL_ID,
    name = "Test session vb",
    vb = TRUE
  )
  skip_if_null_response(result, "create_session with vb = TRUE")

  on.exit(delete_session(vol_id = TEST_VOL_ID, session_id = result$id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
})

test_that("create_session works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- create_session(
    vol_id = TEST_VOL_ID,
    name = "Test session custom rq",
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "create_session with custom_rq")

  on.exit(delete_session(vol_id = TEST_VOL_ID, session_id = result$id, vb = FALSE), add = TRUE)

  expect_type(result, "list")
})

test_that("create_session returns NULL for non-existent volume", {
  expect_null(create_session(vol_id = 999999999, name = "Test", vb = FALSE))
})

test_that("create_session rejects invalid name", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = ""))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "   "))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = 123))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = c("A", "B")))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = NULL))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = NA))
})

test_that("create_session rejects providing both source_date and date", {
  expect_error(
    create_session(
      vol_id = TEST_VOL_ID,
      name = "Test",
      source_date = "2024-03-15",
      date = list(year = 2024, month = 3, day = 15)
    )
  )
})

test_that("create_session rejects malformed source_date", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", source_date = "not-a-date"))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", source_date = ""))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", source_date = 123))
  expect_error(
    create_session(
      vol_id = TEST_VOL_ID,
      name = "Test",
      source_date = as.Date(c("2024-01-01", "2024-02-01"))
    )
  )
})

test_that("create_session rejects malformed date list", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", date = "2024-03-15"))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", date = list(2024, 3, 15)))
})

test_that("create_session rejects invalid release_level", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", release_level = ""))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", release_level = 1))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", release_level = c("A", "B")))
})

test_that("create_session rejects invalid date_precision", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", date_precision = ""))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", date_precision = 1))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", date_precision = c("FULL", "YEAR")))
})

test_that("create_session rejects invalid default_records", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", default_records = "1"))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", default_records = c(1, -2)))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", default_records = c(1, 0)))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", default_records = c(1.5, 2)))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", default_records = integer(0)))
})

test_that("create_session rejects invalid vol_id", {
  expect_error(create_session(vol_id = -1, name = "Test"))
  expect_error(create_session(vol_id = 0, name = "Test"))
  expect_error(create_session(vol_id = "1", name = "Test"))
  expect_error(create_session(vol_id = TRUE, name = "Test"))
  expect_error(create_session(vol_id = list(a = 1), name = "Test"))
  expect_error(create_session(vol_id = c(1, 2), name = "Test"))
  expect_error(create_session(vol_id = 1.5, name = "Test"))
})

test_that("create_session rejects invalid vb parameter", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", vb = -1))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", vb = "a"))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", vb = list(a = 1)))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", vb = c(TRUE, FALSE)))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", vb = NULL))
})

test_that("create_session rejects invalid rq parameter", {
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", rq = "a"))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", rq = -1))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", rq = c(2, 3)))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", rq = list(a = 1)))
  expect_error(create_session(vol_id = TEST_VOL_ID, name = "Test", rq = TRUE))
})
