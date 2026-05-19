# check_duplicate_files_in_session() -------------------------------------------
login_test_account()

test_that("check_duplicate_files_in_session returns a tibble for an empty session", {
  sid <- make_test_session("check_duplicate_files happy path")
  skip_if_null_response(sid, "create_session for check_duplicate_files happy path")

  filenames <- c("nonexistent_a.mp4", "nonexistent_b.mp4")
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    filenames = filenames,
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session(empty session)")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("filename", "exists"))
  expect_equal(nrow(result), length(filenames))
  expect_equal(result$filename, filenames)
  expect_type(result$exists, "logical")
  expect_true(all(!result$exists))
})

test_that("check_duplicate_files_in_session preserves input order", {
  sid <- make_test_session("check_duplicate_files order")
  skip_if_null_response(sid, "create_session for order test")

  filenames <- c("z.mp4", "a.mp4", "m.mp4")
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    filenames = filenames,
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session(order)")

  expect_equal(result$filename, filenames)
})

test_that("check_duplicate_files_in_session works with a single filename", {
  sid <- make_test_session("check_duplicate_files single")
  skip_if_null_response(sid, "create_session for single filename test")

  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    filenames = "only.mp4",
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session(single)")

  expect_equal(nrow(result), 1L)
  expect_equal(result$filename, "only.mp4")
  expect_false(result$exists)
})

test_that("check_duplicate_files_in_session treats missing session like empty (all not found)", {
  # Backend does not require the session to exist; it queries files by session_id only.
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL_ID,
    session_id = TEST_MISSING_ID,
    filenames = c("a.mp4"),
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session(non-existent session)")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$filename, "a.mp4")
  expect_false(result$exists)
})

test_that("check_duplicate_files_in_session works with verbose mode", {
  sid <- make_test_session("check_duplicate_files vb")
  skip_if_null_response(sid, "create_session for check_duplicate_files vb")

  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    filenames = c("vb.mp4"),
    vb = TRUE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session vb")
  expect_s3_class(result, "tbl_df")
})

test_that("check_duplicate_files_in_session works with custom request object", {
  sid <- make_test_session("check_duplicate_files custom rq")
  skip_if_null_response(sid, "create_session for check_duplicate_files custom rq")

  custom_rq <- databraryr::make_default_request()
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    filenames = c("custom.mp4"),
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session custom rq")
  expect_s3_class(result, "tbl_df")
})

test_that("check_duplicate_files_in_session rejects invalid filenames", {
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = character(0)))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = NULL))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = 123))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = list("a")))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = TRUE))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = c("a", NA)))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = c("a", "")))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = c("a", "   ")))
})

test_that("check_duplicate_files_in_session rejects invalid vol_id", {
  expect_error(check_duplicate_files_in_session(vol_id = -1, session_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 0, session_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = "1", session_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = TRUE, session_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = c(1, 2), session_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1.5, session_id = 1, filenames = "a"))
})

test_that("check_duplicate_files_in_session rejects invalid session_id", {
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = -1, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 0, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = "1", filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = TRUE, filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = c(1, 2), filenames = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1.5, filenames = "a"))
})

test_that("check_duplicate_files_in_session rejects invalid vb parameter", {
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = "a", vb = -1))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = "a", vb = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = "a", vb = c(TRUE, FALSE)))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = "a", vb = NULL))
})

test_that("check_duplicate_files_in_session rejects invalid rq parameter", {
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = "a", rq = "a"))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = "a", rq = -1))
  expect_error(check_duplicate_files_in_session(vol_id = 1, session_id = 1, filenames = "a", rq = TRUE))
})
