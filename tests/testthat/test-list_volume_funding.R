# list_volume_funding ---------------------------------------------------------
test_that("list_volume_funding returns data.frame or is NULL", {
  login_test_account()
  result <- list_volume_funding(vol_id = 1)
  skip_if_null_response(result, "list_volume_funding(vol_id = 1)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_volume_funding rejects bad input parameters", {
  expect_error(list_volume_funding(vol_id = "a"))
  expect_error(list_volume_funding(vol_id = TRUE))
  expect_error(list_volume_funding(vol_id = -1))
  
  expect_error(list_volume_funding(vb = -1))
  expect_error(list_volume_funding(vb = 3))
  expect_error(list_volume_funding(vb = "a"))
  expect_error(list_volume_funding(vb = list(a=1, b=2)))
  
  expect_error(list_volume_funding(rq = "a"))
  expect_error(list_volume_funding(rq = -1))
  expect_error(list_volume_funding(rq = c(2,3)))
  expect_error(list_volume_funding(rq = list(a=1, b=2)))
  
})
