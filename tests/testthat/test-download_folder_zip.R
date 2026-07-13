# download_folder_zip ---------------------------------------------------------
test_that("download_folder_zip rejects bad input parameters", {
  expect_error(download_folder_zip(vol_id = -1))
  expect_error(download_folder_zip(vol_id = 0))
  expect_error(download_folder_zip(vol_id = "a"))
  expect_error(download_folder_zip(vol_id = list(a = 1, b = 2)))
  expect_error(download_folder_zip(vol_id = TRUE))

  expect_error(download_folder_zip(folder_id = -1))
  expect_error(download_folder_zip(folder_id = 0))
  expect_error(download_folder_zip(folder_id = "a"))
  expect_error(download_folder_zip(folder_id = list(a = 1, b = 2)))
  expect_error(download_folder_zip(folder_id = TRUE))

  expect_error(download_folder_zip(vb = -1))
  expect_error(download_folder_zip(vb = 3))
  expect_error(download_folder_zip(vb = "a"))
  expect_error(download_folder_zip(vb = list(a = 1, b = 2)))

  expect_error(download_folder_zip(rq = "a"))
  expect_error(download_folder_zip(rq = -1))
  expect_error(download_folder_zip(rq = c(1, 2)))
})

test_that("download_folder_zip returns processing task", {
  captured_path <- NULL
  fake_task <- list(status = "processing", message = "queued", task_id = "xyz")
  task <- with_mocked_bindings(
    download_folder_zip(vol_id = 2, folder_id = 5),
    request_processing_task = function(path, rq = NULL, vb = FALSE) {
      captured_path <<- path
      fake_task
    }
  )

  expect_identical(task, fake_task)
  expect_equal(captured_path, sprintf("/volumes/%s/folders/%s/download-link/", 2, 5))
})



