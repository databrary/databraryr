# patch_session_file() ---------------------------------------------------------
login_test_account()

test_that("patch_session_file returns NULL when no fields provided", {
  expect_null(
    patch_session_file(
      vol_id = TEST_VOL_ID,
      session_id = 1,
      file_id = 1,
      vb = FALSE
    )
  )
})

test_that("patch_session_file returns NULL for non-existent file", {
  result <- patch_session_file(
    vol_id = TEST_VOL_ID,
    session_id = TEST_MISSING_ID,
    file_id = TEST_MISSING_ID,
    name = "nope",
    vb = FALSE
  )
  expect_null(result)
})

test_that("patch_session_file rejects invalid name", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = ""))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "   "))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = 123))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = c("A", "B")))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = NA))
})

test_that("patch_session_file rejects providing both source_date and date", {
  expect_error(
    patch_session_file(
      vol_id = 1,
      session_id = 1,
      file_id = 1,
      source_date = "2024-03-15",
      date = list(year = 2024, month = 3, day = 15)
    )
  )
})

test_that("patch_session_file rejects malformed source_date", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, source_date = "not-a-date"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, source_date = ""))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, source_date = 123))
})

test_that("patch_session_file rejects malformed date", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, date = "2024-03-15"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, date = list(2024, 3, 15)))
})

test_that("patch_session_file rejects invalid release_level", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, release_level = ""))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, release_level = 1))
})

test_that("patch_session_file rejects invalid date_precision", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, date_precision = ""))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, date_precision = 1))
})

test_that("patch_session_file rejects invalid is_estimated", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, is_estimated = "yes"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, is_estimated = c(TRUE, FALSE)))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, is_estimated = 1))
})

test_that("patch_session_file rejects invalid vol_id", {
  expect_error(patch_session_file(vol_id = -1, session_id = 1, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = 0, session_id = 1, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = "1", session_id = 1, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = TRUE, session_id = 1, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = c(1, 2), session_id = 1, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = 1.5, session_id = 1, file_id = 1, name = "x"))
})

test_that("patch_session_file rejects invalid session_id", {
  expect_error(patch_session_file(vol_id = 1, session_id = -1, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = 0, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = "1", file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = TRUE, file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = c(1, 2), file_id = 1, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1.5, file_id = 1, name = "x"))
})

test_that("patch_session_file rejects invalid file_id", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = -1, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 0, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = "1", name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = TRUE, name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = c(1, 2), name = "x"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1.5, name = "x"))
})

test_that("patch_session_file rejects invalid vb parameter", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "x", vb = -1))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "x", vb = "a"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "x", vb = c(TRUE, FALSE)))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "x", vb = NULL))
})

test_that("patch_session_file rejects invalid rq parameter", {
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "x", rq = "a"))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "x", rq = -1))
  expect_error(patch_session_file(vol_id = 1, session_id = 1, file_id = 1, name = "x", rq = TRUE))
})
