# get_volume_by_id ---------------------------------------------------------
test_that("get_volume_by_id returns a list or is NULL.", {
  login_test_account()
  result <- get_volume_by_id(vol_id = 2)
  skip_if_null_response(result, "get_volume_by_id(vol_id = 2)")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$id, 2)
})

test_that("get_volume_by_id rejects bad input parameters", {
  expect_error(get_volume_by_id(vol_id = "a"))
  expect_error(get_volume_by_id(vol_id = -1))
  expect_error(get_volume_by_id(vol_id = c(2,3)))
  expect_error(get_volume_by_id(vol_id = TRUE))

  expect_error(get_volume_by_id(vb = "a"))
  expect_error(get_volume_by_id(vb = -1))
  expect_error(get_volume_by_id(vb = c(2,3)))
  
  expect_error(get_volume_by_id(rq = "a"))
  expect_error(get_volume_by_id(rq = -1))
  expect_error(get_volume_by_id(rq = c(2,3)))
  expect_error(get_volume_by_id(rq = list(a=1, b=2)))
})
