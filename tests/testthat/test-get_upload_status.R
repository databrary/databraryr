# get_upload_status() ----------------------------------------------------------
login_test_account()

TEST_VOL <- 1777

test_that("get_upload_status requires exactly one identifier", {
  expect_error(get_upload_status())
  expect_error(get_upload_status(
    status_url = "https://x/y", upload_guid = "abc"
  ))
})

test_that("get_upload_status validates identifier shape", {
  expect_error(get_upload_status(status_url = ""))
  expect_error(get_upload_status(status_url = c("a", "b")))
  expect_error(get_upload_status(upload_guid = ""))
  expect_error(get_upload_status(upload_guid = 123))
})

test_that("get_upload_status returns NULL for unknown guid", {
  # A well-formed but non-existent GUID should fail cleanly.
  res <- get_upload_status(
    upload_guid = "00000000-0000-0000-0000-000000000000",
    vb = FALSE
  )
  expect_null(res)
})

test_that("get_upload_status returns a status string for a real upload", {
  session <- create_session(
    vol_id = TEST_VOL, name = "get_upload_status test", vb = FALSE
  )
  skip_if_null_response(session, "create_session for get_upload_status")
  on.exit(
    delete_session(vol_id = TEST_VOL, session_id = session$id, vb = FALSE),
    add = TRUE
  )

  init <- initiate_upload(
    filename = "status_probe.mp4",
    destination_type = "session",
    object_id = session$id,
    file_size = 1024L,
    content_type = "video/mp4",
    vb = FALSE
  )
  skip_if_null_response(init, "initiate_upload for get_upload_status")

  status <- get_upload_status(status_url = init$status_url, vb = FALSE)
  skip_if_null_response(status, "get_upload_status by URL")

  expect_type(status, "character")
  expect_length(status, 1)
  expect_true(nzchar(status))
})
