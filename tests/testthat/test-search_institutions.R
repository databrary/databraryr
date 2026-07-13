# search_institutions ---------------------------------------------------------

test_that("search_institutions returns tibble", {
  login_test_account()
  # Empty query: no full-text clause; API lists institutions in the search index
  result <- search_institutions("")
  skip_if_null_response(result, "search_institutions(\"\")")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("institution_id", "score") %in% names(result)))
})

test_that("search_institutions rejects bad queries", {
  expect_error(search_institutions(123))
  expect_error(search_institutions("term", vb = "yes"))
})


