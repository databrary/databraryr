# check_duplicate_files_in_session() -------------------------------------------
login_test_account()

TEST_VOL <- 1777

new_session_id <- function(name = "check_duplicate_files test") {
  created <- create_session(vol_id = TEST_VOL, name = name, vb = FALSE)
  if (is.null(created)) {
    return(NULL)
  }
  created$id
}

test_that("check_duplicate_files_in_session returns a tibble for an empty session", {
  sid <- new_session_id("check_duplicate_files happy path")
  skip_if_null_response(sid, "create_session for check_duplicate_files happy path")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  filenames <- c("nonexistent_a.mp4", "nonexistent_b.mp4")
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL,
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
  sid <- new_session_id("check_duplicate_files order")
  skip_if_null_response(sid, "create_session for order test")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  filenames <- c("z.mp4", "a.mp4", "m.mp4")
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL,
    session_id = sid,
    filenames = filenames,
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session(order)")

  expect_equal(result$filename, filenames)
})

test_that("check_duplicate_files_in_session works with a single filename", {
  sid <- new_session_id("check_duplicate_files single")
  skip_if_null_response(sid, "create_session for single filename test")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL,
    session_id = sid,
    filenames = "only.mp4",
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session(single)")

  expect_equal(nrow(result), 1L)
  expect_equal(result$filename, "only.mp4")
  expect_false(result$exists)
})

test_that("check_duplicate_files_in_session returns NULL for non-existent session", {
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL,
    session_id = 999999999,
    filenames = c("a.mp4"),
    vb = FALSE
  )
  expect_null(result)
})

test_that("check_duplicate_files_in_session works with verbose mode", {
  sid <- new_session_id("check_duplicate_files vb")
  skip_if_null_response(sid, "create_session for check_duplicate_files vb")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL,
    session_id = sid,
    filenames = c("vb.mp4"),
    vb = TRUE
  )
  skip_if_null_response(result, "check_duplicate_files_in_session vb")
  expect_s3_class(result, "tbl_df")
})

test_that("check_duplicate_files_in_session works with custom request object", {
  sid <- new_session_id("check_duplicate_files custom rq")
  skip_if_null_response(sid, "create_session for check_duplicate_files custom rq")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  custom_rq <- databraryr::make_default_request()
  result <- check_duplicate_files_in_session(
    vol_id = TEST_VOL,
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
