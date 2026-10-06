# list_users ------------------------------------------------------------------

test_that("list_users returns tibble for search query", {
  login_test_account()
  result <- list_users(search = "gilmore")
  skip_if_null_response(result, "list_users(search = 'gilmore')")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("user_id", "user_email") %in% names(result)))
})

test_that("list_users rejects bad input parameters", {
  expect_error(list_users(search = 123))
  expect_error(list_users(include_suspended = "yes"))
  expect_error(list_users(exclude_self = c(TRUE, FALSE)))
  expect_error(list_users(is_authorized_investigator = 2))
  expect_error(list_users(has_api_access = list(TRUE)))
  expect_error(list_users(vb = "yes"))
})


