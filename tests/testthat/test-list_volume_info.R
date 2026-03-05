# list_volume_info ------------------------------------------------------------
login_test_account()
test_that("list_volume_info returns tibble for default volume", {
  result <- list_volume_info()
  skip_if_null_response(result, "list_volume_info()")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$vol_id, 1)
  expect_true(all(c("vol_owner_connection", "vol_owner_institution") %in% names(result)))
  expect_true(is.list(result$vol_owner_connection))
  expect_true(is.list(result$vol_owner_institution))
})

login_test_account()
test_that("list_volume_info returns tibble for another volume", {
  result <- list_volume_info(vol_id = 2)
  skip_if_null_response(result, "list_volume_info(vol_id = 2)")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$vol_id, 2)
  expect_true(all(c("vol_owner_connection", "vol_owner_institution") %in% names(result)))
  expect_true(is.list(result$vol_owner_connection))
  expect_true(is.list(result$vol_owner_institution))
})

test_that("list_volume_info rejects bad input parameters", {
  expect_error(list_volume_info(vol_id = "a"))
  expect_error(list_volume_info(vol_id = TRUE))
  expect_error(list_volume_info(vol_id = -1))
  
  expect_error(list_volume_info(vb = -1))
  expect_error(list_volume_info(vb = 3))
  expect_error(list_volume_info(vb = "a"))
  
  expect_error(list_volume_info(rq = -1))
  expect_error(list_volume_info(rq = TRUE))
  expect_error(list_volume_info(rq = c("a", "b")))
  expect_error(list_volume_info(rq = list("a" = 1, "b" = 2)))
})
