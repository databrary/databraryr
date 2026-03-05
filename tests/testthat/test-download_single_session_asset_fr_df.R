# download_session_asset_from_df ---------------------------------------------------------
test_that("download_session_asset_from_df rejects bad input parameters", {
  expect_error(download_session_asset_from_df(i = 0))
  expect_error(download_session_asset_from_df(i = -1))
  expect_error(download_session_asset_from_df(i = "a"))

  expect_error(download_session_asset_from_df(session_df = 3))
  expect_error(download_session_asset_from_df(session_df = "a"))
  expect_error(download_session_asset_from_df(session_df = TRUE))

  missing_cols <- data.frame(vol_id = 1, session_id = 1, asset_id = 1)
  expect_error(download_session_asset_from_df(i = 1, session_df = missing_cols))

  expect_error(download_session_asset_from_df(i = 1, target_dir = 3))
  expect_error(download_session_asset_from_df(i = 1, target_dir = list(a = 1, b = 2)))
  expect_error(download_session_asset_from_df(i = 1, target_dir = TRUE))

  expect_error(download_session_asset_from_df(i = 1, add_session_subdir = -1))
  expect_error(download_session_asset_from_df(i = 1, add_session_subdir = 3))
  expect_error(download_session_asset_from_df(i = 1, add_session_subdir = "a"))
  expect_error(download_session_asset_from_df(i = 1, add_session_subdir = list(a = 1, b = 2)))

  expect_error(download_session_asset_from_df(i = 1, overwrite = -1))
  expect_error(download_session_asset_from_df(i = 1, overwrite = 3))
  expect_error(download_session_asset_from_df(i = 1, overwrite = "a"))
  expect_error(download_session_asset_from_df(i = 1, overwrite = list(a = 1, b = 2)))

  expect_error(download_session_asset_from_df(i = 1, make_portable_fn = -1))
  expect_error(download_session_asset_from_df(i = 1, make_portable_fn = 3))
  expect_error(download_session_asset_from_df(i = 1, make_portable_fn = "a"))
  expect_error(download_session_asset_from_df(i = 1, make_portable_fn = list(a = 1, b = 2)))

  expect_error(download_session_asset_from_df(i = 1, timeout_secs = -1))
  expect_error(download_session_asset_from_df(i = 1, timeout_secs = TRUE))
  expect_error(download_session_asset_from_df(i = 1, timeout_secs = "a"))
  expect_error(download_session_asset_from_df(i = 1, timeout_secs = list(a = 1, b = 2)))

  expect_error(download_session_asset_from_df(i = 1, vb = -1))
  expect_error(download_session_asset_from_df(i = 1, vb = 3))
  expect_error(download_session_asset_from_df(i = 1, vb = "a"))
  expect_error(download_session_asset_from_df(i = 1, vb = list(a = 1, b = 2)))

  expect_error(download_session_asset_from_df(i = 1, rq = "a"))
  expect_error(download_session_asset_from_df(i = 1, rq = -1))
  expect_error(download_session_asset_from_df(i = 1, rq = c(2, 3)))
  expect_error(download_session_asset_from_df(i = 1, rq = list(a = 1, b = 2)))
})
