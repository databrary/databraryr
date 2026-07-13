# list_volume_session_assets --------------------------------------------------
login_test_account()
test_that("list_volume_session_assets returns tibble or is NULL", {
  result <- list_volume_session_assets()
  skip_if_null_response(result, "list_volume_session_assets()")
  expect_s3_class(result, "tbl_df")
})

test_that("list_volume_session_assets returns tibble for accessible session", {
  result <- list_volume_session_assets(vol_id = 2, session_id = 11)
  skip_if_null_response(result, "list_volume_session_assets(vol_id = 2, session_id = 11)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_volume_session_assets rejects bad input parameters", {
  expect_error(list_volume_session_assets(volume_id = "a"))
  expect_error(list_volume_session_assets(volume_id = c(1, 2)))
  expect_error(list_volume_session_assets(volume_id = TRUE))
  expect_error(list_volume_session_assets(volume_id = list(a = 1, b = 2)))
  expect_error(list_volume_session_assets(volume_id = -1))
  
  expect_error(list_volume_session_assets(session_id = "a"))
  expect_error(list_volume_session_assets(session_id = c(1, 2)))
  expect_error(list_volume_session_assets(session_id = TRUE))
  expect_error(list_volume_session_assets(session_id = list(a = 1, b = 2)))
  expect_error(list_volume_session_assets(session_id = -1))
  
  expect_error(list_volume_session_assets(vb = -1))
  expect_error(list_volume_session_assets(vb = 3))
  expect_error(list_volume_session_assets(vb = "a"))
  expect_error(list_volume_session_assets(vb = list(a = 1, b = 2)))
  
  expect_error(list_volume_session_assets(rq = "a"))
  expect_error(list_volume_session_assets(rq = -1))
  expect_error(list_volume_session_assets(rq = c(2, 3)))
  expect_error(list_volume_session_assets(rq = list(a = 1, b = 2)))
})
