test_that("list_user_volumes returns volumes for user 22582", {
  login_test_account()
  result <- list_user_volumes(22582)
  skip_if_null_response(result, "list_user_volumes(22582)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("vol_id", "vol_name", "user_id") %in% names(result)))
})

test_that("list_user_volumes rejects invalid parameters", {
  expect_error(list_user_volumes(user_id = "a"))
  expect_error(list_user_volumes(user_id = -1))
  expect_error(list_user_volumes(user_id = TRUE))
  expect_error(list_user_volumes(user_id = c(1, 2)))
  expect_error(list_user_volumes(user_id = list(a = 1)))

  expect_error(list_user_volumes(rq = 123))
  expect_error(list_user_volumes(rq = list()))
})


