context("list_* functions return types")
library(databraryapi)

# list_people

test_that("list_people returns data.frame", {
  expect_true(class(list_people()) == "data.frame")
})

# list_volume_owners

test_that("list_volume_owners returns data.frame", {
  expect_true(class(list_volume_owners()) == "data.frame")
})

test_that("list_volume_owners rejects bad input parameters", {
  expect_error(list_volume_owners("a"))
  expect_error(list_volume_owners(c(1,2)))
  expect_error(list_volume_owners(TRUE))
  expect_error(list_volume_owners(list(a=1, b=2)))
  expect_error(list_volume_owners(volume = -1))
  expect_error(list_volume_owners(vb = -1))
  expect_error(list_volume_owners(vb = 3))
  expect_error(list_volume_owners(vb = "a"))
})

# list_sessions

test_that("list_sessions returns data.frame", {
  expect_true(class(list_sessions()) == "data.frame")
})

test_that("list_sessions rejects bad input parameters", {
  expect_error(list_sessions("a"))
  expect_error(list_sessions(c(1,2)))
  expect_error(list_sessions(TRUE))
  expect_error(list_sessions(list(a=1, b=2)))
  expect_error(list_sessionss(volume = -1))
  expect_error(list_sessions(vb = -1))
  expect_error(list_sessions(vb = 3))
  expect_error(list_sessions(vb = "a"))
})

# list_assets

test_that("list_assets returns data.frame", {
  expect_true(class(list_assets()) == "data.frame")
})

# list_assets_json

test_that("list_assets_json returns character", {
  expect_true(class(list_assets_json()) == "character")
})

test_that("list_assets_json is JSON", {
  expect_true(jsonlite::validate(list_assets_json()))
})

# list_assets_by_type

test_that("list_assets_by_type returns data.frame", {
  expect_true(class(list_assets_by_type()) == "data.frame")
})
