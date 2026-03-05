# download_folder_asset -------------------------------------------------------
test_that("download_folder_asset rejects bad input parameters", {
  expect_error(download_folder_asset(vol_id = -1))
  expect_error(download_folder_asset(vol_id = 0))
  expect_error(download_folder_asset(vol_id = "a"))
  expect_error(download_folder_asset(vol_id = list(a = 1, b = 2)))
  expect_error(download_folder_asset(vol_id = TRUE))

  expect_error(download_folder_asset(folder_id = -1))
  expect_error(download_folder_asset(folder_id = 0))
  expect_error(download_folder_asset(folder_id = "a"))
  expect_error(download_folder_asset(folder_id = list(a = 1, b = 2)))
  expect_error(download_folder_asset(folder_id = TRUE))

  expect_error(download_folder_asset(asset_id = -1))
  expect_error(download_folder_asset(asset_id = 0))
  expect_error(download_folder_asset(asset_id = "a"))
  expect_error(download_folder_asset(asset_id = list(a = 1, b = 2)))
  expect_error(download_folder_asset(asset_id = TRUE))

  expect_error(download_folder_asset(file_name = 3))
  expect_error(download_folder_asset(file_name = list(a = 1, b = 2)))
  expect_error(download_folder_asset(file_name = TRUE))

  expect_error(download_folder_asset(target_dir = 3))
  expect_error(download_folder_asset(target_dir = list(a = 1, b = 2)))
  expect_error(download_folder_asset(target_dir = TRUE))

  expect_error(download_folder_asset(timeout_secs = -1))
  expect_error(download_folder_asset(timeout_secs = 0))
  expect_error(download_folder_asset(timeout_secs = list(1, 2)))

  expect_error(download_folder_asset(vb = -1))
  expect_error(download_folder_asset(vb = 3))
  expect_error(download_folder_asset(vb = "a"))
  expect_error(download_folder_asset(vb = list(a = 1, b = 2)))

  expect_error(download_folder_asset(rq = "a"))
  expect_error(download_folder_asset(rq = -1))
  expect_error(download_folder_asset(rq = c(2, 3)))
  expect_error(download_folder_asset(rq = list(a = 1, b = 2)))
})

test_that("download_folder_asset fetches signed link", {
  tmp_dir <- tempdir()
  fake_link <- list(download_url = "https://example.com/file.bin", file_name = "example.bin")
  class(fake_link) <- c("databrary_signed_download", "list")

  captured_path <- NULL
  captured_dest <- NULL

  result <- with_mocked_bindings(
    download_folder_asset(vol_id = 1, folder_id = 2, asset_id = 3, target_dir = tmp_dir),
    request_signed_download_link = function(path, rq = NULL, vb = FALSE) {
      captured_path <<- path
      fake_link
    },
    download_signed_file = function(download_url, dest_path, timeout_secs = REQUEST_TIMEOUT, vb = FALSE) {
      captured_dest <<- dest_path
      dest_path
    }
  )
  skip_if_null_response(result, "download_folder_asset(vol_id = 1, folder_id = 2, asset_id = 3, target_dir = tmp_dir)")
  
  expect_true(grepl("example.bin$", result))
  expect_equal(result, captured_dest)
  expect_equal(captured_path, sprintf("/volumes/%s/folders/%s/files/%s/download-link/", 1, 2, 3))
})



