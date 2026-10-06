# list_volume_links ---------------------------------------------------------
login_test_account()

test_that("list_volume_links returns data.frame or is NULL", {
  result <- list_volume_links(vol_id = 1)
  skip_if_null_response(result, "list_volume_links(vol_id = 1)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_volume_links rejects bad input parameters", {
  expect_error(list_volume_links(vol_id = "a"))
  expect_error(list_volume_links(vol_id = c(1,2)))
  expect_error(list_volume_links(vol_id = TRUE))
  expect_error(list_volume_links(vol_id = list(a=1, b=2)))
  expect_error(list_volume_links(vol_id = -1))
  
  expect_error(list_volume_links(vb = -1))
  expect_error(list_volume_links(vb = 3))
  expect_error(list_volume_links(vb = "a"))
  expect_error(list_volume_links(vb = list(a=1, b=2)))
  
  expect_error(list_volume_links(rq = "a"))
  expect_error(list_volume_links(rq = -1))
  expect_error(list_volume_links(rq = c(2,3)))
  expect_error(list_volume_links(rq = list(a=1, b=2)))
})