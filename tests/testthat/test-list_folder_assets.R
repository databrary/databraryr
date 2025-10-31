# list_folder_assets -----------------------------------------------------------
test_that("list_folder_assets returns tibble for accessible folder", {
  login_test_account()
  folders <- list_volume_folders(vol_id = 2)
  skip_if_null_response(folders, "list_volume_folders(vol_id = 2)")

  target_folder <- folders$folder_id[1]
  result <- list_folder_assets(folder_id = target_folder, vol_id = 2)
  skip_if_null_response(result, sprintf("list_folder_assets(folder_id = %s, vol_id = 2)", target_folder))

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$folder_id == target_folder))
})

test_that("list_folder_assets rejects bad input parameters", {
  expect_error(list_folder_assets(folder_id = "a", vol_id = 1))
  expect_error(list_folder_assets(folder_id = c(1, 2), vol_id = 1))
  expect_error(list_folder_assets(folder_id = TRUE, vol_id = 1))
  expect_error(list_folder_assets(folder_id = list(a = 1, b = 2), vol_id = 1))
  expect_error(list_folder_assets(folder_id = -1, vol_id = 1))

  expect_error(list_folder_assets(folder_id = 1))

  expect_error(list_folder_assets(folder_id = 1, vol_id = "a"))
  expect_error(list_folder_assets(folder_id = 1, vol_id = c(1, 2)))
  expect_error(list_folder_assets(folder_id = 1, vol_id = TRUE))
  expect_error(list_folder_assets(folder_id = 1, vol_id = list(a = 1, b = 2)))
  expect_error(list_folder_assets(folder_id = 1, vol_id = -1))

  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, vb = -1))
  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, vb = 3))
  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, vb = "a"))
  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, vb = list(a = 1, b = 2)))

  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, rq = "a"))
  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, rq = -1))
  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, rq = c(2, 3)))
  expect_error(list_folder_assets(folder_id = 1, vol_id = 1, rq = list(a = 1, b = 2)))
})

