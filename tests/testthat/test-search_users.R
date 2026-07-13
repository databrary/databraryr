# search_users ----------------------------------------------------------------

test_that("search_users returns tibble", {
  login_test_account()
  # Empty query: no full-text clause; API lists users in the search index
  result <- search_users("")
  skip_if_null_response(result, "search_users(\"\")")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("user_id", "score") %in% names(result)))
})

test_that("search_users rejects bad queries", {
  expect_error(search_users(123))
  expect_error(search_users("term", vb = "yes"))
})


