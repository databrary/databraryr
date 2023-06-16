context("search_* functions")
library(databrary.r)

# search_for_funder() ---------------------------------------------------
test_that("search_for_funder returns data.frame", {
  if (login_db()) {
    expect_true(class(search_for_funder()) == "data.frame")    
  }
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

# search_for_tags() ---------------------------------------------------
test_that("search_for_tags returns character", {
  expect_true(class(search_for_tags()) == "character")
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
})

# search_for_keywords() ---------------------------------------------------
test_that("search_for_keywords returns list", {
  expect_true(class(search_for_keywords()) == "list")
})

test_that("search_for_keywords rejects bad input parameters", {
  expect_error(search_for_keywords(search_string = -1))
  expect_error(search_for_keywords(search_string = 0))
  expect_error(search_for_keywords(search_string = list(a=1, b=2)))
  expect_error(search_for_keywords(search_string = TRUE))

  expect_error(search_for_keywords(vb = -1))
  expect_error(search_for_keywords(vb = 3))
  expect_error(search_for_keywords(vb = "a"))
  expect_error(search_for_keywords(vb = list(a=1, b=2)))
})
