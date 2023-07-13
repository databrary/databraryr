# list_people ----------------------------------------------------------------
test_that("list_people returns data.frame", {
  expect_true("data.frame" %in% class(list_people()))
})

test_that("list_people rejects bad input parameters", {
  expect_error(list_people(people_list = "a"))
  expect_error(list_people(people_list = TRUE))
  expect_error(list_people(people_list = 0))
  expect_error(list_people(people_list = -1))
  
  expect_error(list_people(vb = -1))
  expect_error(list_people(vb = 3))
  expect_error(list_people(vb = "a"))
  expect_error(list_people(vb = list(a=1, b=2)))
})