context("Databrary constants")
library(databraryapi)

test_that("assign_constants returns list", {
  expect_true(class(assign_constants()) == "list")
})

test_that("get_supported_file_types returns data.frame", {
  expect_true(class(get_supported_file_types()) == "data.frame")
})

test_that("get_permission_levels returns character", {
  expect_true(class(get_permission_levels()) == "character")
})

test_that("get_release_levels returns character", {
 expect_true(class(get_release_levels()) == "character")
})
