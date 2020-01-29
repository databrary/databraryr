context("get_* functions")
library(databraryapi)

# get_asset_segment_range ------------------------------------
test_that("get_asset_segment_range returns an integer array", {
  expect_true(class(get_asset_segment_range()) == "integer")
  expect_true(length(get_asset_segment_range()) == 2)
})

test_that("get_asset_segment_range rejects bad input parameters", {
  expect_error(get_asset_segment_range(vol_id = "a"))
  expect_error(get_asset_segment_range(vol_id = -1))
  expect_error(get_asset_segment_range(vol_id = c(1,3)))

  expect_error(get_asset_segment_range(session_id = "a"))
  expect_error(get_asset_segment_range(session_id = -1))
  expect_error(get_asset_segment_range(session_id = c(1,3)))

  expect_error(get_asset_segment_range(asset_id = "a"))
  expect_error(get_asset_segment_range(asset_id = -1))
  expect_error(get_asset_segment_range(asset_id = c(1,3)))

  expect_error(get_asset_segment_range(vb = "a"))
  expect_error(get_asset_segment_range(vb = -1))
  expect_error(get_asset_segment_range(vb = c(2,3)))
})

# get_db_stats ---------------------------------------------------------

test_that("get_db_stats returns a data.frame", {
  expect_true(class(get_db_stats())=="data.frame")
})

test_that("get_db_stats rejects bad input parameters", {
  expect_error(get_db_stats(type = "a"))
  expect_error(get_db_stats(type = -1))
  expect_error(get_db_stats(type = c(1,2)))

  expect_error(get_db_stats(vb = -1))
  expect_error(get_db_stats(vb = 3))
  expect_error(get_db_stats(vb = "a"))
  expect_error(get_db_stats(vb = list(a=1, b=2)))
})

# get_file_duration ---------------------------------------------------------

test_that("get_file_duration returns an integer array", {
  expect_true(class(get_file_duration()) == "integer")
  expect_true(length(get_file_duration()) == 1)
})

test_that("get_file_duration rejects bad input parameters", {
  expect_error(get_file_duration(asset_id = "a"))
  expect_error(get_file_duration(asset_id = -1))
  expect_error(get_file_duration(asset_id = c(1,3)))

  expect_error(get_file_duration(vb = "a"))
  expect_error(get_file_duration(vb = -1))
  expect_error(get_file_duration(vb = c(2,3)))
})

# get_permission_levels ---------------------------------------------------------

test_that("get_permission_levels returns a character array", {
  expect_true(class(get_permission_levels()) == "character")
  expect_true(length(get_permission_levels()) == 6)
})

test_that("get_permission_levels rejects bad input parameters", {
  expect_error(get_permission_levels(vb = "a"))
  expect_error(get_permission_levels(vb = -1))
  expect_error(get_permission_levels(vb = c(2,3)))
})

# get_release_levels ---------------------------------------------------------

test_that("get_release_levels returns a character array", {
  expect_true(class(get_release_levels()) == "character")
  expect_true(length(get_release_levels()) == 4)
})

test_that("get_release_levels rejects bad input parameters", {
  expect_error(get_release_levels(vb = "a"))
  expect_error(get_release_levels(vb = -1))
  expect_error(get_release_levels(vb = c(2,3)))
})

# get_supported_file_types ---------------------------------------------------------

test_that("get_supported_file_types returns data.frame", {
  expect_true(class(get_supported_file_types()) == "data.frame")
})

test_that("get_supported_file_types rejects bad input parameters", {
  expect_error(get_supported_file_types(vb = -1))
  expect_error(get_supported_file_types(vb = 3))
  expect_error(get_supported_file_types(vb = "a"))
})

# get_video_stats ---------------------------------------------------------

test_that("get_video_stats returns a data.frame", {
  expect_true(class(get_video_stats()) == "data.frame")
  #expect_true(dim(get_video_stats()) == c(1,4))
})

test_that("get_video_stats rejects bad input parameters", {
  expect_error(get_video_stats(vol_id = "a"))
  expect_error(get_video_stats(vol_id = -1))
  expect_error(get_video_stats(vol_id = c(1,3)))

  expect_error(get_video_stats(vb = "a"))
  expect_error(get_video_stats(vb = -1))
  expect_error(get_video_stats(vb = c(2,3)))
})

# get_individual_sponsors ---------------------------------------------------------
test_that("get_individual_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(get_individual_sponsors()) ||
                 (class(get_individual_sponsors()) == "data.frame")))
})

test_that("get_individual_sponsors rejects bad input parameters", {
  expect_error(get_individual_sponsors(party_id = "a"))
  expect_error(get_individual_sponsors(party_id = -1))
  expect_error(get_individual_sponsors(party_id = TRUE))
  expect_error(get_individual_sponsors(party_id = c(1,3)))
  expect_error(get_individual_sponsors(party_id = list(a=1, b=2)))

  expect_error(get_individual_sponsors(vb = "a"))
  expect_error(get_individual_sponsors(vb = -1))
  expect_error(get_individual_sponsors(vb = c(2,3)))
  expect_error(get_individual_sponsors(vb = list(a=1, b=2)))
})

# get_institutional_sponsors ---------------------------------------------------------
test_that("get_institutional_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(get_institutional_sponsors()) ||
                 (class(get_institutional_sponsors()) == "data.frame")))
})

test_that("get_institutional_sponsors rejects bad input parameters", {
  expect_error(get_institutional_sponsors(party_id = "a"))
  expect_error(get_institutional_sponsors(party_id = -1))
  expect_error(get_institutional_sponsors(party_id = TRUE))
  expect_error(get_institutional_sponsors(party_id = c(1,3)))
  expect_error(get_institutional_sponsors(party_id = list(a=1, b=2)))

  expect_error(get_institutional_sponsors(vb = "a"))
  expect_error(get_institutional_sponsors(vb = -1))
  expect_error(get_institutional_sponsors(vb = c(2,3)))
  expect_error(get_institutional_sponsors(vb = list(a=1, b=2)))
})

# get_affiliates ---------------------------------------------------------
test_that("get_affiliates returns a data.frame or is NULL.", {
  expect_true((is.null(get_affiliates()) ||
                 (class(get_affiliates()) == "data.frame")))
})

test_that("get_affiliates rejects bad input parameters", {
  expect_error(get_affiliates(party_id = "a"))
  expect_error(get_affiliates(party_id = -1))
  expect_error(get_affiliates(party_id = TRUE))
  expect_error(get_affiliates(party_id = c(1,3)))
  expect_error(get_affiliates(party_id = list(a=1, b=2)))

  expect_error(get_affiliates(vb = "a"))
  expect_error(get_affiliates(vb = -1))
  expect_error(get_affiliates(vb = c(2,3)))
  expect_error(get_affiliates(vb = list(a=1, b=2)))
})

# get_sponsors ---------------------------------------------------------
test_that("get_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(get_sponsors()) ||
                 (class(get_sponsors()) == "data.frame")))
})

test_that("get_sponsors rejects bad input parameters", {
  expect_error(get_sponsors(party_id = "a"))
  expect_error(get_sponsors(party_id = -1))
  expect_error(get_sponsors(party_id = TRUE))
  expect_error(get_sponsors(party_id = c(1,3)))
  expect_error(get_sponsors(party_id = list(a=1, b=2)))

  expect_error(get_sponsors(vb = "a"))
  expect_error(get_sponsors(vb = -1))
  expect_error(get_sponsors(vb = c(2,3)))
  expect_error(get_sponsors(vb = list(a=1, b=2)))
})
