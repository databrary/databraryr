# download_folder_asset_from_df ----------------------------------------
test_that("download_folder_asset_from_df rejects bad input parameters", {
  expect_error(download_folder_asset_from_df(i = 0))
  expect_error(download_folder_asset_from_df(i = -1))
  expect_error(download_folder_asset_from_df(i = "a"))

  expect_error(download_folder_asset_from_df(i = 1, folder_df = 3))
  expect_error(download_folder_asset_from_df(i = 1, folder_df = "a"))
  expect_error(download_folder_asset_from_df(i = 1, folder_df = TRUE))

  missing_cols <- data.frame(vol_id = 1, folder_id = 1, asset_id = 1)
  expect_error(download_folder_asset_from_df(i = 1, folder_df = missing_cols))

  expect_error(download_folder_asset_from_df(i = 1, target_dir = 3))
  expect_error(download_folder_asset_from_df(i = 1, target_dir = list(a = 1, b = 2)))
  expect_error(download_folder_asset_from_df(i = 1, target_dir = TRUE))

  expect_error(download_folder_asset_from_df(i = 1, add_folder_subdir = -1))
  expect_error(download_folder_asset_from_df(i = 1, add_folder_subdir = 3))
  expect_error(download_folder_asset_from_df(i = 1, add_folder_subdir = "a"))
  expect_error(download_folder_asset_from_df(i = 1, add_folder_subdir = list(a = 1, b = 2)))

  expect_error(download_folder_asset_from_df(i = 1, overwrite = -1))
  expect_error(download_folder_asset_from_df(i = 1, overwrite = 3))
  expect_error(download_folder_asset_from_df(i = 1, overwrite = "a"))
  expect_error(download_folder_asset_from_df(i = 1, overwrite = list(a = 1, b = 2)))

  expect_error(download_folder_asset_from_df(i = 1, make_portable_fn = -1))
  expect_error(download_folder_asset_from_df(i = 1, make_portable_fn = 3))
  expect_error(download_folder_asset_from_df(i = 1, make_portable_fn = "a"))
  expect_error(download_folder_asset_from_df(i = 1, make_portable_fn = list(a = 1, b = 2)))

  expect_error(download_folder_asset_from_df(i = 1, timeout_secs = -1))
  expect_error(download_folder_asset_from_df(i = 1, timeout_secs = TRUE))
  expect_error(download_folder_asset_from_df(i = 1, timeout_secs = "a"))
  expect_error(download_folder_asset_from_df(i = 1, timeout_secs = list(a = 1, b = 2)))

  expect_error(download_folder_asset_from_df(i = 1, vb = -1))
  expect_error(download_folder_asset_from_df(i = 1, vb = 3))
  expect_error(download_folder_asset_from_df(i = 1, vb = "a"))
  expect_error(download_folder_asset_from_df(i = 1, vb = list(a = 1, b = 2)))

  expect_error(download_folder_asset_from_df(i = 1, rq = "a"))
  expect_error(download_folder_asset_from_df(i = 1, rq = -1))
  expect_error(download_folder_asset_from_df(i = 1, rq = c(2, 3)))
  expect_error(download_folder_asset_from_df(i = 1, rq = list(a = 1, b = 2)))
})

test_that("download_folder_asset_from_df delegates to download_folder_asset", {
  folder_df <- tibble::tibble(
    vol_id = 1,
    folder_id = 2,
    asset_id = 3,
    asset_name = "demo",
    format_extension = "txt"
  )

  captured <- NULL
  result <- with_mocked_bindings(
    download_folder_asset_from_df(i = 1, folder_df = folder_df, target_dir = tempdir()),
    download_folder_asset = function(vol_id, folder_id, asset_id, file_name, target_dir, ...) {
      captured <<- list(
        vol_id = vol_id,
        folder_id = folder_id,
        asset_id = asset_id,
        file_name = file_name,
        target_dir = target_dir
      )
      "downloaded-path"
    }
  )

  expect_equal(result, "downloaded-path")
  expect_equal(captured$vol_id, 1)
  expect_equal(captured$folder_id, 2)
  expect_equal(captured$asset_id, 3)
  expect_true(grepl("demo.txt$", captured$file_name))
})

