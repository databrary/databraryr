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

# list_sessions

test_that("list_sessions returns data.frame", {
  expect_true(class(list_sessions()) == "data.frame")
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
