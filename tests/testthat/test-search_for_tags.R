# search_for_tags() ---------------------------------------------------
test_that("search_for_tags returns tagged volumes", {
  login_test_account()
  result <- search_for_tags("ICIS")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("search_for_tags rejects bad input parameters", {
  expect_error(search_for_tags(search_string = -1))
  expect_error(search_for_tags(search_string = 0))
  expect_error(search_for_tags(search_string = list(a=1, b=2)))
  expect_error(search_for_tags(search_string = TRUE))
  
  expect_error(search_for_tags(vb = -1))
  expect_error(search_for_tags(vb = 3))
  expect_error(search_for_tags(vb = "a"))
  expect_error(search_for_tags(vb = list(a=1, b=2)))
  
  expect_error(search_for_tags(rq = "a"))
  expect_error(search_for_tags(rq = -1))
  expect_error(search_for_tags(rq = c(2,3)))
  expect_error(search_for_tags(rq = list(a=1, b=2)))
})
