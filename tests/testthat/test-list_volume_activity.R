# list_volume_activity ---------------------------------------------------------
test_that("list_volume_activity returns data.frame or is NULL", {
  login_test_account()
  result <- list_volume_activity(vol_id = 1892)
  skip_if_null_response(result, "list_volume_activity(vol_id = 1892)")
  expect_s3_class(result, "tbl_df")
})

test_that("list_volume_activity rejects bad input parameters", {
  expect_error(list_volume_activity(vol_id = "a"))
  expect_error(list_volume_activity(vol_id = c(1,2)))
  expect_error(list_volume_activity(vol_id = TRUE))
  expect_error(list_volume_activity(vol_id = list(a=1, b=2)))
  expect_error(list_volume_activity(vol_id = -1))
  
  expect_error(list_volume_activity(vb = -1))
  expect_error(list_volume_activity(vb = 3))
  expect_error(list_volume_activity(vb = "a"))
  expect_error(list_volume_activity(vb = list(a=1, b=2)))
})