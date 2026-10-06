# download_session_csv ---------------------------------------------------------
test_that("download_session_csv rejects bad input parameters", {
  expect_error(download_session_csv(vol_id = -1))
  expect_error(download_session_csv(vol_id = 0))
  expect_error(download_session_csv(vol_id = "a"))
  expect_error(download_session_csv(vol_id = list(a = 1, b = 2)))
  expect_error(download_session_csv(vol_id = TRUE))

  expect_error(download_session_csv(session_id = -1))
  expect_error(download_session_csv(session_id = 0))
  expect_error(download_session_csv(session_id = "a"))
  expect_error(download_session_csv(session_id = list(a = 1, b = 2)))
  expect_error(download_session_csv(session_id = TRUE))

  expect_error(download_session_csv(vb = -1))
  expect_error(download_session_csv(vb = 3))
  expect_error(download_session_csv(vb = "a"))
  expect_error(download_session_csv(vb = list(a = 1, b = 2)))

  expect_error(download_session_csv(rq = "a"))
  expect_error(download_session_csv(rq = -1))
  expect_error(download_session_csv(rq = c(1, 2)))
})

test_that("download_session_csv returns volume processing task", {
  captured_path <- NULL
  fake_task <- list(status = "processing", message = "queued", task_id = "abc")
  task <- with_mocked_bindings(
    download_session_csv(),
    request_processing_task = function(path, rq = NULL, vb = FALSE) {
      captured_path <<- path
      fake_task
    }
  )

  expect_identical(task, fake_task)
  expect_equal(captured_path, sprintf("/volumes/%s/csv-download-link/", 1))
})

test_that("download_session_csv returns session processing task", {
  captured_path <- NULL
  fake_task <- list(status = "processing", message = "queued", task_id = "def")
  task <- with_mocked_bindings(
    download_session_csv(vol_id = 2, session_id = 11),
    request_processing_task = function(path, rq = NULL, vb = FALSE) {
      captured_path <<- path
      fake_task
    }
  )

  expect_identical(task, fake_task)
  expect_equal(captured_path, sprintf("/volumes/%s/sessions/%s/csv-download-link/", 2, 11))
})
