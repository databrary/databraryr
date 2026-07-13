test_that("get_institution_by_id returns institution metadata", {
  login_test_account()
  result <- get_institution_by_id(1)
  skip_if_null_response(result, "get_institution_by_id(1)")
  expect_true(is.list(result))
  expect_equal(result$id, 1)
  expect_equal(result$name, "Databrary")
})



