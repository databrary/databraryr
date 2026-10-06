login_test_account()

test_that("assign_constants returns constants", {
  result <- assign_constants()
  skip_if_null_response(result, "assign_constants()")
  expect_true(is.list(result))
  expect_true("format_df" %in% names(result))
  expect_s3_class(result$format_df, "tbl_df")
  expect_gt(nrow(result$format_df), 0)
})

test_that("assign_constants rejects bad input parameters", {
  expect_error(assign_constants(vb = -1))
  expect_error(assign_constants(vb = 3))
  expect_error(assign_constants(vb = "a"))
  
  expect_error(assign_constants(rq = -1))
  expect_error(assign_constants(rq = 3))
  expect_error(assign_constants(rq = "a"))
})

test_that("assign_constants returns permission metadata", {
  result <- assign_constants()
  skip_if_null_response(result, "assign_constants() metadata")
  expect_true("permission" %in% names(result))
  expect_true("release" %in% names(result))
  expect_true("volume_access_levels" %in% names(result$permission))
  expect_true(length(result$permission$volume_access_levels) > 0)
})
