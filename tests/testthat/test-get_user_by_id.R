test_that("get_user_by_id returns user metadata", {
  login_test_account()
  result <- get_user_by_id(22582)
  skip_if_null_response(result, "get_user_by_id(22582)")
  expect_true(is.list(result))
  expect_equal(result$id, 22582)
  expect_true(grepl("Armatys", result$sortname))
})

