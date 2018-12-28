context("download_* functions")
library(databraryapi)

# download_containers_records ---------------------------------------------------
test_that("download_containers_records returns list", {
  expect_true(class(download_containers_records()) == "list")
})

test_that("download_containers_records rejects bad input parameters", {
  expect_error(download_containers_records(vol_id = -1))
  expect_error(download_containers_records(vol_id = 0))
  expect_error(download_containers_records(vol_id = "a"))
  expect_error(download_containers_records(vol_id = list(a=1, b=2)))
  expect_error(download_containers_records(vol_id = TRUE))

  expect_error(download_containers_records(convert_JSON = -1))
  expect_error(download_containers_records(convert_JSON = 3))
  expect_error(download_containers_records(convert_JSON = "a"))
  expect_error(download_containers_records(convert_JSON = list(a=1, b=2)))

  expect_error(download_containers_records(vb = -1))
  expect_error(download_containers_records(vb = 3))
  expect_error(download_containers_records(vb = "a"))
  expect_error(download_containers_records(vb = list(a=1, b=2)))
})

# download_datavyu ---------------------------------------------------
# test_that("download_datavyu rejects bad input parameters", {
#   expect_error(download_datavyu(vol_id = -1))
#   expect_error(download_datavyu(vol_id = "a"))
#   expect_error(download_datavyu(vol_id = list(a=1, b=2)))
#   expect_error(download_datavyu(vol_id = TRUE))
#
#   expect_error(download_datavyu(session_id = -1))
#   expect_error(download_datavyu(session_id = "a"))
#   expect_error(download_datavyu(session_id = list(a=1, b=2)))
#   expect_error(download_datavyu(session_id = TRUE))
#
#   expect_error(download_datavyu(asset_id = -1))
#   expect_error(download_datavyu(asset_id = "a"))
#   expect_error(download_datavyu(asset_id = list(a=1, b=2)))
#   expect_error(download_datavyu(asset_id = TRUE))
#
#   expect_error(download_datavyu(out_dir = -1))
#   expect_error(download_datavyu(out_dir = list(a=1, b=2)))
#   expect_error(download_datavyu(out_dir = TRUE))
#
#   expect_error(download_datavyu(file_name = -1))
#   expect_error(download_datavyu(file_name = list(a=1, b=2)))
#   expect_error(download_datavyu(file_name = TRUE))
#
#   expect_error(download_datavyu(return_response = -1))
#   expect_error(download_datavyu(return_response = 3))
#   expect_error(download_datavyu(return_response = "a"))
#   expect_error(download_datavyu(return_response = list(a=1, b=2)))
#
#   expect_error(download_datavyu(vb = -1))
#   expect_error(download_datavyu(vb = 3))
#   expect_error(download_datavyu(vb = "a"))
#   expect_error(download_datavyu(vb = list(a=1, b=2)))
# })

# download_video ---------------------------------------------------
# test_that("download_video rejects bad input parameters", {
#   expect_error(download_video(vol_id = -1))
#   expect_error(download_video(vol_id = "a"))
#   expect_error(download_video(vol_id = list(a=1, b=2)))
#   expect_error(download_video(vol_id = TRUE))
#
#   expect_error(download_video(session_id = -1))
#   expect_error(download_video(session_id = "a"))
#   expect_error(download_video(session_id = list(a=1, b=2)))
#   expect_error(download_video(session_id = TRUE))
#
#   expect_error(download_video(asset_id = -1))
#   expect_error(download_video(asset_id = "a"))
#   expect_error(download_video(asset_id = list(a=1, b=2)))
#   expect_error(download_video(asset_id = TRUE))
#
#   expect_error(download_video(segment_id = -1))
#   expect_error(download_video(segment_id = list(a=1, b=2)))
#   expect_error(download_video(segment_id = TRUE))
#
#   expect_error(download_video(out_dir = -1))
#   expect_error(download_video(out_dir = list(a=1, b=2)))
#   expect_error(download_video(out_dir = TRUE))
#
#   expect_error(download_video(file_name = -1))
#   expect_error(download_video(file_name = list(a=1, b=2)))
#   expect_error(download_video(file_name = TRUE))
#
#   expect_error(download_video(return_response = -1))
#   expect_error(download_video(return_response = 3))
#   expect_error(download_video(return_response = "a"))
#   expect_error(download_video(return_response = list(a=1, b=2)))
#
#   expect_error(download_video(vb = -1))
#   expect_error(download_video(vb = 3))
#   expect_error(download_video(vb = "a"))
#   expect_error(download_video(vb = list(a=1, b=2)))
# })
