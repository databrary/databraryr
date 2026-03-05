# get_folder_by_id -------------------------------------------------------------
test_that("get_folder_by_id returns folder metadata", {
  login_test_account()
  folders <- list_volume_folders(vol_id = 2)
  skip_if_null_response(folders, "list_volume_folders(vol_id = 2)")

  target_folder <- folders$folder_id[1]
  result <- get_folder_by_id(folder_id = target_folder, vol_id = 2)
  skip_if_null_response(result, sprintf("get_folder_by_id(folder_id = %s, vol_id = 2)", target_folder))

  expect_type(result, "list")
  expect_equal(result$id, target_folder)
})

test_that("get_folder_by_id rejects bad input parameters", {
  expect_error(get_folder_by_id(folder_id = "a"))
  expect_error(get_folder_by_id(folder_id = c(1, 2)))
  expect_error(get_folder_by_id(folder_id = TRUE))
  expect_error(get_folder_by_id(folder_id = list(a = 1, b = 2)))
  expect_error(get_folder_by_id(folder_id = -1))

  expect_error(get_folder_by_id(vol_id = "a"))
  expect_error(get_folder_by_id(vol_id = c(1, 2)))
  expect_error(get_folder_by_id(vol_id = TRUE))
  expect_error(get_folder_by_id(vol_id = list(a = 1, b = 2)))
  expect_error(get_folder_by_id(vol_id = -1))

  expect_error(get_folder_by_id(vb = -1))
  expect_error(get_folder_by_id(vb = 3))
  expect_error(get_folder_by_id(vb = "a"))
  expect_error(get_folder_by_id(vb = list(a = 1, b = 2)))

  expect_error(get_folder_by_id(rq = "a"))
  expect_error(get_folder_by_id(rq = -1))
  expect_error(get_folder_by_id(rq = c(2, 3)))
  expect_error(get_folder_by_id(rq = list(a = 1, b = 2)))
})

