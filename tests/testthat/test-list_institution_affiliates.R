test_that("list_institution_affiliates returns affiliates for institution 1", {
  login_test_account()
  result <- list_institution_affiliates(1)
  skip_if_null_response(result, "list_institution_affiliates(1)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})



