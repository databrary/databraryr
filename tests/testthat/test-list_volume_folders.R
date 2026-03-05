# list_volume_folders ----------------------------------------------------------
test_that("list_volume_folders returns tibble for accessible volume", {
  login_test_account()
  result <- list_volume_folders(vol_id = 2)
  skip_if_null_response(result, "list_volume_folders(vol_id = 2)")
  expect_s3_class(result, "tbl_df")
})

test_that("list_volume_folders rejects bad input parameters", {
  expect_error(list_volume_folders(vol_id = "a"))
  expect_error(list_volume_folders(vol_id = c(1, 2)))
  expect_error(list_volume_folders(vol_id = TRUE))
  expect_error(list_volume_folders(vol_id = list(a = 1, b = 2)))
  expect_error(list_volume_folders(vol_id = -1))

  expect_error(list_volume_folders(vb = -1))
  expect_error(list_volume_folders(vb = 3))
  expect_error(list_volume_folders(vb = "a"))
  expect_error(list_volume_folders(vb = list(a = 1, b = 2)))

  expect_error(list_volume_folders(rq = "a"))
  expect_error(list_volume_folders(rq = -1))
  expect_error(list_volume_folders(rq = c(2, 3)))
  expect_error(list_volume_folders(rq = list(a = 1, b = 2)))
})

