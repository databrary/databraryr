# search_institutions ---------------------------------------------------------

test_that("search_institutions returns tibble", {
  login_test_account()
  result <- search_institutions("state")
  skip_if_null_response(result, "search_institutions('state')")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("institution_id", "score") %in% names(result)))
})

test_that("search_institutions rejects bad queries", {
  expect_error(search_institutions(123))
  expect_error(search_institutions("term", vb = "yes"))
})


