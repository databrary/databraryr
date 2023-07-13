# search_for_funder() ---------------------------------------------------
test_that("search_for_funder returns data.frame", {
 expect_true("data.frame" %in% class(search_for_funder()))
})

test_that("search_for_funder rejects bad input parameters", {
  expect_error(search_for_funder(search_string = -1))
  expect_error(search_for_funder(search_string = 0))
  expect_error(search_for_funder(search_string = list(a=1, b=2)))
  expect_error(search_for_funder(search_string = TRUE))
  
  expect_error(search_for_funder(vb = -1))
  expect_error(search_for_funder(vb = 3))
  expect_error(search_for_funder(vb = "a"))
  expect_error(search_for_funder(vb = list(a=1, b=2)))
})
