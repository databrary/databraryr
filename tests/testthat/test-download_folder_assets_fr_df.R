# download_folder_assets_fr_df -----------------------------------------------
test_that("download_folder_assets_fr_df rejects bad input parameters", {
  expect_error(download_folder_assets_fr_df(folder_df = 3))
  expect_error(download_folder_assets_fr_df(folder_df = "a"))
  expect_error(download_folder_assets_fr_df(folder_df = TRUE))

  missing_cols <- data.frame(vol_id = 1, folder_id = 1, asset_id = 1)
  expect_error(download_folder_assets_fr_df(folder_df = missing_cols))

  expect_error(download_folder_assets_fr_df(target_dir = 3))
  expect_error(download_folder_assets_fr_df(target_dir = list(a = 1, b = 2)))
  expect_error(download_folder_assets_fr_df(target_dir = TRUE))

  expect_error(download_folder_assets_fr_df(add_folder_subdir = -1))
  expect_error(download_folder_assets_fr_df(add_folder_subdir = 3))
  expect_error(download_folder_assets_fr_df(add_folder_subdir = "a"))
  expect_error(download_folder_assets_fr_df(add_folder_subdir = list(a = 1, b = 2)))

  expect_error(download_folder_assets_fr_df(overwrite = -1))
  expect_error(download_folder_assets_fr_df(overwrite = 3))
  expect_error(download_folder_assets_fr_df(overwrite = "a"))
  expect_error(download_folder_assets_fr_df(overwrite = list(a = 1, b = 2)))

  expect_error(download_folder_assets_fr_df(make_portable_fn = -1))
  expect_error(download_folder_assets_fr_df(make_portable_fn = 3))
  expect_error(download_folder_assets_fr_df(make_portable_fn = "a"))
  expect_error(download_folder_assets_fr_df(make_portable_fn = list(a = 1, b = 2)))

  expect_error(download_folder_assets_fr_df(timeout_secs = -1))
  expect_error(download_folder_assets_fr_df(timeout_secs = TRUE))
  expect_error(download_folder_assets_fr_df(timeout_secs = "a"))
  expect_error(download_folder_assets_fr_df(timeout_secs = list(a = 1, b = 2)))

  expect_error(download_folder_assets_fr_df(vb = -1))
  expect_error(download_folder_assets_fr_df(vb = 3))
  expect_error(download_folder_assets_fr_df(vb = "a"))
  expect_error(download_folder_assets_fr_df(vb = list(a = 1, b = 2)))

  expect_error(download_folder_assets_fr_df(rq = "a"))
  expect_error(download_folder_assets_fr_df(rq = -1))
  expect_error(download_folder_assets_fr_df(rq = c(1, 2)))
  expect_error(download_folder_assets_fr_df(rq = list(a = 1, b = 2)))
})

test_that("download_folder_assets_fr_df iterates rows", {
  folder_df <- tibble::tibble(
    vol_id = c(1, 1),
    folder_id = c(2, 2),
    asset_id = c(3, 4),
    asset_name = c("file_a", "file_b")
  )

  calls <- list()
  results <- with_mocked_bindings(
    download_folder_assets_fr_df(folder_df = folder_df, target_dir = tempdir(), vb = FALSE),
    download_folder_asset_from_df = function(i, folder_df, ...) {
      calls[[length(calls) + 1]] <<- list(i = i, folder_df = folder_df)
      paste0("path-", i)
    }
  )

  expect_equal(results, c("path-1", "path-2"))
  expect_equal(vapply(calls, function(x) x$i, numeric(1)), c(1, 2))
})

