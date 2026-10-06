# get_session_file -------------------------------------------------------
test_that("get_session_file returns file metadata", {
  login_test_account()
  files <- list_volume_session_assets(vol_id = 2, session_id = 11)
  skip_if_null_response(files, "list_volume_session_assets(vol_id = 2, session_id = 11)")

  target_file <- files$asset_id[1]
  result <- get_session_file(vol_id = 2, session_id = 11, file_id = target_file)
  skip_if_null_response(result, sprintf("get_session_file(vol_id = 2, session_id = 11, file_id = %s)", target_file))

  expect_type(result, "list")
  expect_equal(result$id, target_file)
  expect_true(all(c("id", "name") %in% names(result)))
})

test_that("get_session_file rejects bad input parameters", {
  expect_error(get_session_file(vol_id = "a", file_id = 1))
  expect_error(get_session_file(vol_id = c(1, 2), file_id = 1))
  expect_error(get_session_file(vol_id = TRUE, file_id = 1))
  expect_error(get_session_file(vol_id = list(a = 1), file_id = 1))
  expect_error(get_session_file(vol_id = -1, file_id = 1))

  expect_error(get_session_file(session_id = "a", file_id = 1))
  expect_error(get_session_file(session_id = c(1, 2), file_id = 1))
  expect_error(get_session_file(session_id = TRUE, file_id = 1))
  expect_error(get_session_file(session_id = list(a = 1), file_id = 1))
  expect_error(get_session_file(session_id = -1, file_id = 1))

  expect_error(get_session_file(file_id = "a"))
  expect_error(get_session_file(file_id = c(1, 2)))
  expect_error(get_session_file(file_id = TRUE))
  expect_error(get_session_file(file_id = list(a = 1)))
  expect_error(get_session_file(file_id = -1))

  expect_error(get_session_file(file_id = 1, vb = -1))
  expect_error(get_session_file(file_id = 1, vb = 3))
  expect_error(get_session_file(file_id = 1, vb = "a"))
  expect_error(get_session_file(file_id = 1, vb = list(a = 1)))

  expect_error(get_session_file(file_id = 1, rq = "a"))
  expect_error(get_session_file(file_id = 1, rq = -1))
  expect_error(get_session_file(file_id = 1, rq = c(2, 3)))
  expect_error(get_session_file(file_id = 1, rq = list(a = 1)))
})
