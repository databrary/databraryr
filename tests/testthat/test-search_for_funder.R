# search_for_funder() ---------------------------------------------------
login_test_account()
test_that("search_for_funder finds matching funder", {
  result <- search_for_funder("National Science Foundation")
  skip_if_null_response(result, "search_for_funder(\"National Science Foundation\")")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("National Science Foundation", result$funder_name, fixed = TRUE)))
})

test_that("search_for_funder rejects bad input parameters", {
  expect_error(search_for_funder(search_string = -1))
  expect_error(search_for_funder(search_string = 0))
  expect_error(search_for_funder(search_string = list(a = 1, b = 2)))
  expect_error(search_for_funder(search_string = TRUE))
  
  expect_error(search_for_funder(vb = -1))
  expect_error(search_for_funder(vb = 3))
  expect_error(search_for_funder(vb = "a"))
  expect_error(search_for_funder(vb = list(a = 1, b = 2)))
  
  expect_error(search_for_funder(rq = "a"))
  expect_error(search_for_funder(rq = -1))
  expect_error(search_for_funder(rq = c(2, 3)))
  expect_error(search_for_funder(rq = list(a = 1, b = 2)))
})
