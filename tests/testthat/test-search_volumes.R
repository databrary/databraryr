# search_volumes --------------------------------------------------------------

test_that("search_volumes returns tibble", {
  login_test_account()
  # Empty query: no full-text clause; API returns public volumes in the search index
  result <- search_volumes("")
  skip_if_null_response(result, "search_volumes(\"\")")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("volume_id", "score") %in% names(result)))
})

test_that("search_volumes rejects bad queries", {
  expect_error(search_volumes(123))
  expect_error(search_volumes("term", vb = "yes"))
})


