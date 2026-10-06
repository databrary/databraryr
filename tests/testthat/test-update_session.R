# update_session() -------------------------------------------------------------
login_test_account()

test_that("update_session replaces name via PUT", {
  sid <- make_test_session("update_session original")
  skip_if_null_response(sid, "create_session for update_session name test")

  result <- update_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "update_session replaced",
    vb = FALSE
  )
  skip_if_null_response(result, "update_session(name=...)")

  expect_type(result, "list")
  expect_equal(result$name, "update_session replaced")
  expect_equal(as.integer(result$id), as.integer(sid))
})

test_that("update_session replaces source_date with a Date object", {
  sid <- make_test_session("update_session source_date Date")
  skip_if_null_response(sid, "create_session for update_session source_date Date")

  result <- update_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "update_session source_date Date",
    source_date = as.Date("2024-03-15"),
    vb = FALSE
  )
  skip_if_null_response(result, "update_session(source_date=Date)")
  expect_type(result, "list")
})

test_that("update_session returns NULL for non-existent session", {
  result <- update_session(
    vol_id = TEST_VOL_ID,
    session_id = TEST_MISSING_ID,
    name = "nope",
    vb = FALSE
  )
  expect_null(result)
})

test_that("update_session works with verbose mode", {
  sid <- make_test_session("update_session vb")
  skip_if_null_response(sid, "create_session for update_session vb")

  result <- update_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "update_session vb replaced",
    vb = TRUE
  )
  skip_if_null_response(result, "update_session vb")
  expect_type(result, "list")
})

test_that("update_session works with custom request object", {
  sid <- make_test_session("update_session custom rq")
  skip_if_null_response(sid, "create_session for update_session custom rq")

  custom_rq <- databraryr::make_default_request()
  result <- update_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "update_session custom rq replaced",
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "update_session custom rq")
  expect_type(result, "list")
})

test_that("update_session rejects missing/invalid name", {
  expect_error(update_session(vol_id = 1, session_id = 1))
  expect_error(update_session(vol_id = 1, session_id = 1, name = ""))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "   "))
  expect_error(update_session(vol_id = 1, session_id = 1, name = 123))
  expect_error(update_session(vol_id = 1, session_id = 1, name = c("A", "B")))
  expect_error(update_session(vol_id = 1, session_id = 1, name = NULL))
  expect_error(update_session(vol_id = 1, session_id = 1, name = NA))
})

test_that("update_session rejects providing both source_date and date", {
  expect_error(
    update_session(
      vol_id = 1,
      session_id = 1,
      name = "x",
      source_date = "2024-03-15",
      date = list(year = 2024, month = 3, day = 15)
    )
  )
})

test_that("update_session rejects malformed source_date", {
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", source_date = "not-a-date"))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", source_date = ""))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", source_date = 123))
})

test_that("update_session rejects malformed date", {
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", date = "2024-03-15"))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", date = list(2024, 3, 15)))
})

test_that("update_session rejects invalid release_level", {
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", release_level = ""))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", release_level = 1))
})

test_that("update_session rejects invalid date_precision", {
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", date_precision = ""))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", date_precision = 1))
})

test_that("update_session rejects invalid default_records", {
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", default_records = "1"))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", default_records = c(1, -2)))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", default_records = c(1, 0)))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", default_records = c(1.5, 2)))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", default_records = integer(0)))
})

test_that("update_session rejects invalid vol_id", {
  expect_error(update_session(vol_id = -1, session_id = 1, name = "x"))
  expect_error(update_session(vol_id = 0, session_id = 1, name = "x"))
  expect_error(update_session(vol_id = "1", session_id = 1, name = "x"))
  expect_error(update_session(vol_id = TRUE, session_id = 1, name = "x"))
  expect_error(update_session(vol_id = c(1, 2), session_id = 1, name = "x"))
  expect_error(update_session(vol_id = 1.5, session_id = 1, name = "x"))
})

test_that("update_session rejects invalid session_id", {
  expect_error(update_session(vol_id = 1, session_id = -1, name = "x"))
  expect_error(update_session(vol_id = 1, session_id = 0, name = "x"))
  expect_error(update_session(vol_id = 1, session_id = "1", name = "x"))
  expect_error(update_session(vol_id = 1, session_id = TRUE, name = "x"))
  expect_error(update_session(vol_id = 1, session_id = c(1, 2), name = "x"))
  expect_error(update_session(vol_id = 1, session_id = 1.5, name = "x"))
})

test_that("update_session rejects invalid vb parameter", {
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", vb = -1))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", vb = "a"))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", vb = c(TRUE, FALSE)))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", vb = NULL))
})

test_that("update_session rejects invalid rq parameter", {
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", rq = "a"))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", rq = -1))
  expect_error(update_session(vol_id = 1, session_id = 1, name = "x", rq = TRUE))
})
