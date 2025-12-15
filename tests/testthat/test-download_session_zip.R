#download_session_zip ---------------------------------------------------
test_that("download_session_zip rejects bad input parameters", {
  expect_error(download_session_zip(vol_id = -1))
  expect_error(download_session_zip(vol_id = "a"))
  expect_error(download_session_zip(vol_id = list(a = 1, b = 2)))
  expect_error(download_session_zip(vol_id = TRUE))

  expect_error(download_session_zip(session_id = -1))
  expect_error(download_session_zip(session_id = "a"))
  expect_error(download_session_zip(session_id = list(a = 1, b = 2)))
  expect_error(download_session_zip(session_id = TRUE))

  expect_error(download_session_zip(vb = -1))
  expect_error(download_session_zip(vb = 3))
  expect_error(download_session_zip(vb = "a"))
  expect_error(download_session_zip(vb = list(a = 1, b = 2)))

  expect_error(download_session_zip(rq = "a"))
  expect_error(download_session_zip(rq = -1))
  expect_error(download_session_zip(rq = c(1, 2)))
})

test_that("download_session_zip returns processing task", {
  captured_path <- NULL
  fake_task <- list(status = "processing", message = "queued", task_id = "abc")
  task <- with_mocked_bindings(
    download_session_zip(),
    request_processing_task = function(path, rq = NULL, vb = FALSE) {
      captured_path <<- path
      fake_task
    }
  )

  expect_identical(task, fake_task)
  expect_equal(
    captured_path,
    sprintf("/volumes/%s/sessions/%s/download-link/", 31, 9803)
  )
})
