# list_user_history -----------------------------------------------------------

test_that("list_user_history returns tibble", {
  login_test_account()
  result <- list_user_history(user_id = 22582)
  skip_if_null_response(result, "list_user_history(user_id = 22582)")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("history_id", "history_type", "history_timestamp") %in% names(result)))
})

test_that("list_user_history rejects bad input parameters", {
  expect_error(list_user_history(user_id = "a"))
  expect_error(list_user_history(user_id = c(1, 2)))
  expect_error(list_user_history(user_id = -1))
  expect_error(list_user_history(vb = "yes"))
})


