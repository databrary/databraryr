library(databraryr)

test_that("download_session_csv(vol_id = 3) returns NULL", {
  expect_true(is.null(download_session_csv(3)))
})

# download_session_csv ---------------------------------------------------------
test_that("download_session_csv rejects bad input parameters", {
  expect_error(download_session_csv(vol_id = -1))
  expect_error(download_session_csv(vol_id = 0))
  expect_error(download_session_csv(vol_id = "a"))
  expect_error(download_session_csv(vol_id = list(a=1, b=2)))
  expect_error(download_session_csv(vol_id = TRUE))
  
  expect_error(download_session_csv(file_name = 3))
  expect_error(download_session_csv(file_name = list(a=1, b=2)))
  expect_error(download_session_csv(file_name = TRUE))
  
  expect_error(download_session_csv(target_dir = 3))
  expect_error(download_session_csv(target_dir = list(a=1, b=2)))
  expect_error(download_session_csv(target_dir = TRUE))
  
  expect_error(download_session_csv(return_response = -1))
  expect_error(download_session_csv(return_response = 3))
  expect_error(download_session_csv(return_response = "a"))
  expect_error(download_session_csv(return_response = list(a=1, b=2)))
  
  expect_error(download_session_csv(vb = -1))
  expect_error(download_session_csv(vb = 3))
  expect_error(download_session_csv(vb = "a"))
  expect_error(download_session_csv(vb = list(a=1, b=2)))
})


#download_session_zip ---------------------------------------------------
test_that("download_session_zip rejects bad input parameters", {
  expect_error(download_session_zip(vol_id = -1))
  expect_error(download_session_zip(vol_id = "a"))
  expect_error(download_session_zip(vol_id = list(a=1, b=2)))
  expect_error(download_session_zip(vol_id = TRUE))

  expect_error(download_session_zip(session_id = -1))
  expect_error(download_session_zip(session_id = "a"))
  expect_error(download_session_zip(session_id = list(a=1, b=2)))
  expect_error(download_session_zip(session_id = TRUE))
  
  expect_error(download_session_zip(out_dir = -1))
  expect_error(download_session_zip(out_dir = list(a=1, b=2)))
  expect_error(download_session_zip(out_dir = TRUE))

  expect_error(download_session_zip(file_name = -1))
  expect_error(download_session_zip(file_name = list(a=1, b=2)))
  expect_error(download_session_zip(file_name = TRUE))
  
  expect_error(download_session_zip(vb = -1))
  expect_error(download_session_zip(vb = 3))
  expect_error(download_session_zip(vb = "a"))
  expect_error(download_session_zip(vb = list(a=1, b=2)))
})

#download_volume_zip ---------------------------------------------------
test_that("download_volume_zip rejects bad input parameters", {
  expect_error(download_volume_zip(vol_id = -1))
  expect_error(download_volume_zip(vol_id = "a"))
  expect_error(download_volume_zip(vol_id = list(a=1, b=2)))
  expect_error(download_volume_zip(vol_id = TRUE))
  
  expect_error(download_volume_zip(session_id = -1))
  expect_error(download_volume_zip(session_id = "a"))
  expect_error(download_volume_zip(session_id = list(a=1, b=2)))
  expect_error(download_volume_zip(session_id = TRUE))
  
  expect_error(download_volume_zip(out_dir = -1))
  expect_error(download_volume_zip(out_dir = list(a=1, b=2)))
  expect_error(download_volume_zip(out_dir = TRUE))
  
  expect_error(download_volume_zip(file_name = -1))
  expect_error(download_volume_zip(file_name = list(a=1, b=2)))
  expect_error(download_volume_zip(file_name = TRUE))
  
  expect_error(download_volume_zip(vb = -1))
  expect_error(download_volume_zip(vb = 3))
  expect_error(download_volume_zip(vb = "a"))
  expect_error(download_volume_zip(vb = list(a=1, b=2)))
})
