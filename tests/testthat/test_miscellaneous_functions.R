library(databraryr)

# format_to_filetypes ---------------------------------------------------
# Can't test return value without input parameter
# test_that("format_to_filetypes returns data.frame", {
#   expect_true(class(format_to_filetypes()) == "data.frame")
# })

test_that("format_to_filetypes rejects bad input parameters", {
  expect_error(format_to_filetypes(vol_assets = -1))
  expect_error(format_to_filetypes(vol_assets = 0))
  expect_error(format_to_filetypes(vol_assets = "a"))
  expect_error(format_to_filetypes(vol_assets = list(a=1, b=2)))
  expect_error(format_to_filetypes(vol_assets = TRUE))

  expect_error(format_to_filetypes(vb = -1))
  expect_error(format_to_filetypes(vb = 3))
  expect_error(format_to_filetypes(vb = "a"))
  expect_error(format_to_filetypes(vb = list(a=1, b=2)))
})

# HHMMSSmmm_to_ms ---------------------------------------------------
test_that("HHMMSSmmm_to_ms returns number", {
  expect_true(class(HHMMSSmmm_to_ms()) == "numeric")
})

test_that("HHMMSSmmm_to_ms rejects bad input parameters", {
  expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = -1))
  #expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = "a"))
  #expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = list(a=1, b=2)))
  expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = TRUE))
})

# is_institution ---------------------------------------------------
test_that("is_institution returns logical", {
  expect_true(class(is_institution()) == "logical")
})

test_that("is_institution rejects bad input parameters", {
  expect_error(is_institution(party_id = -1))
  expect_error(is_institution(party_id = "a"))
  expect_error(is_institution(party_id = list(a=1, b=2)))
  expect_error(is_institution(party_id = TRUE))
})

# read_csv_data_as_df ---------------------------------------------------
test_that("read_csv_data_as_df returns data.frame", {
  expect_true(class(read_csv_data_as_df()) == "data.frame")
})

test_that("read_csv_data_as_df rejects bad input parameters", {
  expect_error(read_csv_data_as_df(session_id = -1))
  expect_error(read_csv_data_as_df(session_id = "a"))
  expect_error(read_csv_data_as_df(session_id = list(a=1, b=2)))
  expect_error(read_csv_data_as_df(session_id = TRUE))

  expect_error(read_csv_data_as_df(asset_id = -1))
  expect_error(read_csv_data_as_df(asset_id = "a"))
  expect_error(read_csv_data_as_df(asset_id = list(a=1, b=2)))
  expect_error(read_csv_data_as_df(asset_id = TRUE))

  expect_error(read_csv_data_as_df(vb = -1))
  expect_error(read_csv_data_as_df(vb = 3))
  expect_error(read_csv_data_as_df(vb = "a"))
  expect_error(read_csv_data_as_df(vb = list(a=1, b=2)))
})
