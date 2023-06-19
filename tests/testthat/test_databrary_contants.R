library(databraryr)

# assign_constants

test_that("assign_constants returns list", {
  expect_true(class(assign_constants()) == "list")
})

test_that("assign_constants rejects bad input parameters", {
  expect_error(assign_constants(vb = -1))
  expect_error(assign_constants(vb = 3))
  expect_error(assign_constants(vb = "a"))
})
