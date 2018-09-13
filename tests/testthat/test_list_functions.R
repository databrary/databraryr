context("list_* functions return types")
library(databraryapi)

# list_people

test_that("list_people returns data.frame", {
  expect_true(class(list_people()) == "data.frame")
})

test_that("list_people rejects bad input parameters", {
  expect_error(list_people(people.list = "a"))
  expect_error(list_people(people.list = TRUE))
  expect_error(list_people(people.list = 0))
  expect_error(list_people(people.list = -1))
  expect_error(list_people(vb = -1))
  expect_error(list_people(vb = 3))
  expect_error(list_people(vb = "a"))
  expect_error(list_people(vb = list(a=1, b=2)))
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
  expect_error(list_volume_owners(vb = list(a=1, b=2)))
})

# list_sessions

test_that("list_sessions returns data.frame", {
  expect_true(class(list_sessions()) == "data.frame")
})

test_that("list_sessions rejects bad input parameters", {
  expect_error(list_sessions(volume = "a"))
#  expect_error(list_sessions(volume = c(1,2)))
  expect_error(list_sessions(volume = TRUE))
  expect_error(list_sessions(volume = list(a=1, b=2)))
  expect_error(list_sessionss(volume = -1))
  expect_error(list_sessions(vb = -1))
  expect_error(list_sessions(vb = 3))
  expect_error(list_sessions(vb = "a"))
})

# list_assets

test_that("list_assets returns data.frame", {
  expect_true(class(list_assets()) == "data.frame")
})

test_that("list_assets rejects bad input parameters", {
  expect_error(list_assets(slot = "a"))
  expect_error(list_assets(slot = c(1,2)))
  expect_error(list_assets(slot = TRUE))
  expect_error(list_assets(slot = list(a=1, b=2)))
  expect_error(list_assets(volume = -1))
  expect_error(list_assets(volume = 0))
  expect_error(list_assets(vb = -1))
  expect_error(list_assets(vb = 3))
  expect_error(list_assets(vb = "a"))
  expect_error(list_assets(vb = list(a=1, b=2)))
})
# list_assets_json

test_that("list_assets_json returns character", {
  expect_true(class(list_assets_json()) == "character")
})

test_that("list_assets_json is JSON", {
  expect_true(jsonlite::validate(list_assets_json()))
})

test_that("list_assets_by_type rejects bad input parameters", {
  expect_error(list_assets_by_type(slot = "a"))
  expect_error(list_assets_by_type(slot = c(1,2)))
  expect_error(list_assets_by_type(slot = TRUE))
  expect_error(list_assets_by_type(slot = list(a=1, b=2)))
  expect_error(list_assets_by_type(volume = -1))
  expect_error(list_assets_by_type(volume = 0))
  expect_error(list_assets_by_type(vb = -1))
  expect_error(list_assets_by_type(vb = 3))
  expect_error(list_assets_by_type(vb = "a"))
  expect_error(list_assets_by_type(vb = list(a=1, b=2)))
})

# list_assets_by_type

# test_that("list_assets_by_type returns data.frame", {
#   expect_true(class(list_assets_by_type()) == "data.frame")
# })

test_that("list_assets_by_type rejects bad input parameters", {
  expect_error(list_assets_by_type(volume = "a"))
#  expect_error(list_assets_by_type(volume = c(1,2)))
  expect_error(list_assets_by_type(volume = TRUE))
  expect_error(list_assets_by_type(volume = list(a=1, b=2)))
  expect_error(list_assets_by_type(volume = -1))
  expect_error(list_assets_by_type(volume = 0))
  expect_error(list_assets_by_type(type = 1))
#  expect_error(list_assets_by_type(type = "a"))
#  expect_error(list_assets_by_type(type = list(a=1, b=2)))
  expect_error(list_assets_by_type(type = TRUE))
  expect_error(list_assets_by_type(vb = -1))
  expect_error(list_assets_by_type(vb = 3))
  expect_error(list_assets_by_type(vb = "a"))
  expect_error(list_assets_by_type(vb = list(a=1, b=2)))
})

# list_containers_records_json

test_that("list_containers_records_json returns character", {
  expect_true(class(list_containers_records_json()) == "character")
})

test_that("list_containers_records_json is JSON", {
  expect_true(jsonlite::validate(list_containers_records_json()))
})

test_that("list_containers_records_json rejects bad input parameters", {
  expect_error(list_containers_records_json(volume = "a"))
  expect_error(list_containers_records_json(volume = c(1,2)))
  expect_error(list_containers_records_json(volume = TRUE))
  expect_error(list_containers_records_json(volume = list(a=1, b=2)))
  expect_error(list_containers_records_json(volume = -1))
  expect_error(list_containers_records_json(volume = 0))
  expect_error(list_containers_records_json(vb = -1))
  expect_error(list_containers_records_json(vb = 3))
  expect_error(list_containers_records_json(vb = "a"))
#  expect_error(list_containers_records_json(vb = list(a=1, b=2)))
})
