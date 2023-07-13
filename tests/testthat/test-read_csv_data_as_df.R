# read_csv_data_as_df ---------------------------------------------------
test_that("read_csv_data_as_df returns data.frame", {
  expect_true("data.frame" %in% class(read_csv_data_as_df()))
})

test_that("read_csv_data_as_df rejects bad input parameters", {
  expect_error(read_csv_data_as_df(session_id = -1))
  expect_error(read_csv_data_as_df(session_id = "a"))
  expect_error(read_csv_data_as_df(session_id = list(a = 1, b = 2)))
  expect_error(read_csv_data_as_df(session_id = TRUE))
  
  expect_error(read_csv_data_as_df(asset_id = -1))
  expect_error(read_csv_data_as_df(asset_id = "a"))
  expect_error(read_csv_data_as_df(asset_id = list(a = 1, b = 2)))
  expect_error(read_csv_data_as_df(asset_id = TRUE))
  
  expect_error(read_csv_data_as_df(vb = -1))
  expect_error(read_csv_data_as_df(vb = 3))
  expect_error(read_csv_data_as_df(vb = "a"))
  expect_error(read_csv_data_as_df(vb = list(a = 1, b = 2)))
})
