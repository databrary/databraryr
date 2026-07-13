test_that("list_user_affiliates returns affiliates for user 22582", {
  login_test_account()
  result <- list_user_affiliates(22582)
  skip_if_null_response(result, "list_user_affiliates(22582)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c(
    "affiliate_user",
    "access_level",
    "expiration_date"
  ) %in% names(result)))
  expect_true(is.list(result$affiliate_user))
})

test_that("list_user_affiliates rejects invalid parameters", {
  expect_error(list_user_affiliates(user_id = "a"))
  expect_error(list_user_affiliates(user_id = -1))
  expect_error(list_user_affiliates(user_id = TRUE))
  expect_error(list_user_affiliates(user_id = c(1, 2)))
  expect_error(list_user_affiliates(user_id = list(a = 1)))

  expect_error(list_user_affiliates(rq = 123))
  expect_error(list_user_affiliates(rq = list()))
})


