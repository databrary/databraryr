# patch_session() --------------------------------------------------------------
login_test_account()

test_that("patch_session updates name", {
  sid <- make_test_session("patch_session original")
  skip_if_null_response(sid, "create_session for patch_session name test")

  result <- patch_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "patch_session renamed",
    vb = FALSE
  )
  skip_if_null_response(result, "patch_session(name=...)")

  expect_type(result, "list")
  expect_equal(result$name, "patch_session renamed")
  expect_equal(as.integer(result$id), as.integer(sid))
})

test_that("patch_session updates source_date with a Date object", {
  sid <- make_test_session("patch_session source_date Date")
  skip_if_null_response(sid, "create_session for patch_session source_date Date")

  result <- patch_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    source_date = as.Date("2024-03-15"),
    vb = FALSE
  )
  skip_if_null_response(result, "patch_session(source_date=Date)")
  expect_type(result, "list")
})

test_that("patch_session returns NULL when no fields provided", {
  expect_null(patch_session(vol_id = TEST_VOL_ID, session_id = 1, vb = FALSE))
})

test_that("patch_session returns NULL for non-existent session", {
  result <- patch_session(
    vol_id = TEST_VOL_ID,
    session_id = 999999999,
    name = "nope",
    vb = FALSE
  )
  expect_null(result)
})

test_that("patch_session works with verbose mode", {
  sid <- make_test_session("patch_session vb")
  skip_if_null_response(sid, "create_session for patch_session vb")

  result <- patch_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "patch_session vb renamed",
    vb = TRUE
  )
  skip_if_null_response(result, "patch_session vb")
  expect_type(result, "list")
})

test_that("patch_session works with custom request object", {
  sid <- make_test_session("patch_session custom rq")
  skip_if_null_response(sid, "create_session for patch_session custom rq")

  custom_rq <- databraryr::make_default_request()
  result <- patch_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    name = "patch_session custom rq renamed",
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "patch_session custom rq")
  expect_type(result, "list")
})

test_that("patch_session rejects invalid name", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = ""))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "   "))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = 123))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = c("A", "B")))
})

test_that("patch_session rejects providing both source_date and date", {
  expect_error(
    patch_session(
      vol_id = TEST_VOL_ID,
      session_id = 1,
      source_date = "2024-03-15",
      date = list(year = 2024, month = 3, day = 15)
    )
  )
})

test_that("patch_session rejects malformed source_date", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, source_date = "not-a-date"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, source_date = ""))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, source_date = 123))
})

test_that("patch_session rejects malformed date", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, date = "2024-03-15"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, date = list(2024, 3, 15)))
})

test_that("patch_session rejects invalid release_level", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, release_level = ""))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, release_level = 1))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, release_level = c("A", "B")))
})

test_that("patch_session rejects invalid date_precision", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, date_precision = ""))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, date_precision = 1))
})

test_that("patch_session rejects invalid default_records", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, default_records = "1"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, default_records = c(1, -2)))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, default_records = c(1, 0)))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, default_records = c(1.5, 2)))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, default_records = integer(0)))
})

test_that("patch_session rejects invalid vol_id", {
  expect_error(patch_session(vol_id = -1, session_id = 1, name = "x"))
  expect_error(patch_session(vol_id = 0, session_id = 1, name = "x"))
  expect_error(patch_session(vol_id = "1", session_id = 1, name = "x"))
  expect_error(patch_session(vol_id = TRUE, session_id = 1, name = "x"))
  expect_error(patch_session(vol_id = c(1, 2), session_id = 1, name = "x"))
  expect_error(patch_session(vol_id = 1.5, session_id = 1, name = "x"))
})

test_that("patch_session rejects invalid session_id", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = -1, name = "x"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 0, name = "x"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = "1", name = "x"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = TRUE, name = "x"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = c(1, 2), name = "x"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1.5, name = "x"))
})

test_that("patch_session rejects invalid vb parameter", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "x", vb = -1))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "x", vb = "a"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "x", vb = c(TRUE, FALSE)))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "x", vb = NULL))
})

test_that("patch_session rejects invalid rq parameter", {
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "x", rq = "a"))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "x", rq = -1))
  expect_error(patch_session(vol_id = TEST_VOL_ID, session_id = 1, name = "x", rq = TRUE))
})
