# list_volume_sessions --------------------------------------------------------
test_that("list_volume_sessions returns tibble given valid vol_id", {
  login_test_account()
  result <- list_volume_sessions()
  skip_if_null_response(result, "list_volume_sessions()")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_volume_sessions returns tibble for another volume", {
  login_test_account()
  result <- list_volume_sessions(vol_id = 2)
  skip_if_null_response(result, "list_volume_sessions(vol_id = 2)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_volume_sessions returns NULL for unknown volume", {
  login_test_account()
  expect_null(list_volume_sessions(vol_id = 9999))
})

test_that("list_volume_sessions rejects bad input parameters", {
  expect_error(list_volume_sessions(vol_id = "a"))
  expect_error(list_volume_sessions(vol_id = TRUE))
  expect_error(list_volume_sessions(vol_id = -1))
  
  expect_error(list_volume_sessions(include_vol_data = -1))
  expect_error(list_volume_sessions(include_vol_data = 3))
  expect_error(list_volume_sessions(include_vol_data = "a"))
  
  expect_error(list_volume_sessions(vb = -1))
  expect_error(list_volume_sessions(vb = 3))
  expect_error(list_volume_sessions(vb = "a"))
  
  expect_error(list_volume_sessions(rq = -1))
  expect_error(list_volume_sessions(rq = TRUE))
  expect_error(list_volume_sessions(rq = c("a", "b")))
  expect_error(list_volume_sessions(rq = list("a" = 1, "b" = 2)))
})
