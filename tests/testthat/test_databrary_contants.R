context("Databrary constants")
library(databraryapi)

# assign_constants

test_that("assign_constants returns list", {
  expect_true(class(assign_constants()) == "list")
})

test_that("assign_constants rejects bad input parameters", {
  expect_error(assign_constants(vb = -1))
  expect_error(assign_constants(vb = 3))
  expect_error(assign_constants(vb = "a"))
})

# get_supported_file_types

test_that("get_supported_file_types returns data.frame", {
  expect_true(class(get_supported_file_types()) == "data.frame")
})

test_that("get_supported_file_types rejects bad input parameters", {
  expect_error(get_supported_file_types(vb = -1))
  expect_error(get_supported_file_types(vb = 3))
  expect_error(get_supported_file_types(vb = "a"))
})

# get_permission_levels

test_that("get_permission_levels returns character", {
  expect_true(class(get_permission_levels()) == "character")
})

test_that("get_permission_levels rejects bad input parameters", {
  expect_error(get_permission_levels(vb = -1))
  expect_error(get_permission_levels(vb = 3))
  expect_error(get_permission_levels(vb = "a"))
})

# get_release_levels

test_that("get_release_levels returns character", {
 expect_true(class(get_release_levels()) == "character")
})

test_that("get_permission_levels rejects bad input parameters", {
  expect_error(get_release_levels(vb = -1))
  expect_error(get_release_levels(vb = 3))
  expect_error(get_release_levels(vb = "a"))
})
