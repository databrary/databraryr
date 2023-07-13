# list_volume_metadata --------------------------------------------------------------
test_that("list_volume_metadata returns data.frame", {
  expect_true(class(list_volume_metadata()) == "data.frame")
})

test_that("list_volume_metadata rejects bad input parameters", {
  expect_error(list_volume_metadata(vol_id = "a"))
  expect_error(list_volume_metadata(vol_id = TRUE))
  expect_error(list_volume_metadata(vol_id = list(a=1, b=2)))
  expect_error(list_volume_metadata(vol_id = -1))
  
  expect_error(list_volume_metadata(write_header = -1))
  expect_error(list_volume_metadata(write_header = 3))
  expect_error(list_volume_metadata(write_header = "a"))
  expect_error(list_volume_metadata(write_header = list(a=1, b=2)))
  
  expect_error(list_volume_metadata(data_frame = -1))
  expect_error(list_volume_metadata(data_frame = 3))
  expect_error(list_volume_metadata(data_frame = "a"))
  expect_error(list_volume_metadata(data_frame = list(a=1, b=2)))
  
  expect_error(list_volume_metadata(vb = -1))
  expect_error(list_volume_metadata(vb = 3))
  expect_error(list_volume_metadata(vb = "a"))
  expect_error(list_volume_metadata(vb = list(a=1, b=2)))
})
