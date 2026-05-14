# delete_session_file() --------------------------------------------------------
login_test_account()

test_that("delete_session_file returns FALSE for non-existent file", {
  expect_false(
    delete_session_file(
      vol_id = 1777,
      session_id = 999999999,
      file_id = 999999999,
      vb = FALSE
    )
  )
})

test_that("delete_session_file rejects invalid vol_id", {
  expect_error(delete_session_file(vol_id = -1, session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = 0, session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = "1", session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = TRUE, session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = list(a = 1), session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = c(1, 2), session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = 1.5, session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = NULL, session_id = 1, file_id = 1))
  expect_error(delete_session_file(vol_id = NA, session_id = 1, file_id = 1))
})

test_that("delete_session_file rejects invalid session_id", {
  expect_error(delete_session_file(vol_id = 1777, session_id = -1, file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = 0, file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = "1", file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = TRUE, file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = list(a = 1), file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = c(1, 2), file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1.5, file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = NULL, file_id = 1))
  expect_error(delete_session_file(vol_id = 1777, session_id = NA, file_id = 1))
})

test_that("delete_session_file rejects invalid file_id", {
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = -1))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 0))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = "1"))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = TRUE))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = list(a = 1)))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = c(1, 2)))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1.5))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = NULL))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = NA))
})

test_that("delete_session_file rejects invalid vb parameter", {
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, vb = -1))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, vb = 3))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, vb = "a"))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, vb = list(a = 1)))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, vb = c(TRUE, FALSE)))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, vb = NULL))
})

test_that("delete_session_file rejects invalid rq parameter", {
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, rq = "a"))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, rq = -1))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, rq = c(2, 3)))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, rq = list(a = 1)))
  expect_error(delete_session_file(vol_id = 1777, session_id = 1, file_id = 1, rq = TRUE))
})
