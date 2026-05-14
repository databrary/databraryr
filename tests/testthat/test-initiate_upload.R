# initiate_upload() ------------------------------------------------------------
login_test_account()

test_that("initiate_upload validates required args", {
  expect_error(initiate_upload(
    filename = "", destination_type = "session", object_id = 1
  ))
  expect_error(initiate_upload(
    filename = NULL, destination_type = "session", object_id = 1
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "", object_id = 1
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 0
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = "1"
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 1.5
  ))
})

test_that("initiate_upload rejects optional args of wrong shape", {
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 1,
    file_size = -1
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 1,
    file_size = "100"
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 1,
    content_type = ""
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 1,
    content_type = c("video/mp4", "video/quicktime")
  ))
})

test_that("initiate_upload requires source_session_id for linked_volume_session", {
  expect_error(initiate_upload(
    filename = "a.mp4",
    destination_type = "linked_volume_session",
    object_id = 1
  ))
})

test_that("initiate_upload requires source_folder_id for linked_volume_folder", {
  expect_error(initiate_upload(
    filename = "a.mp4",
    destination_type = "linked_volume_folder",
    object_id = 1
  ))
})

test_that("initiate_upload rejects invalid vb / rq", {
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 1,
    vb = "yes"
  ))
  expect_error(initiate_upload(
    filename = "a.mp4", destination_type = "session", object_id = 1,
    rq = "not-a-request"
  ))
})

test_that("initiate_upload returns signed url and status_url for a session", {
  sid <- make_test_session("initiate_upload happy path")
  skip_if_null_response(sid, "create_session for initiate_upload happy path")

  result <- initiate_upload(
    filename = "test_clip.mp4",
    destination_type = "session",
    object_id = sid,
    file_size = 1024L,
    content_type = "video/mp4",
    vb = FALSE
  )
  skip_if_null_response(result, "initiate_upload happy path")

  expect_type(result, "list")
  expect_true(!is.null(result$status_url))
  expect_true(!is.null(result$upload_type))
  # Either a single PUT with signed_upload_url, or multipart with part_urls
  if (identical(result$upload_type, "multipart")) {
    expect_true(!is.null(result$part_urls))
    expect_true(!is.null(result$s3_upload_id))
    expect_true(!is.null(result$upload_guid))
  } else {
    expect_true(!is.null(result$signed_upload_url))
  }
})

test_that("initiate_upload returns NULL for nonexistent destination", {
  expect_null(initiate_upload(
    filename = "test.mp4",
    destination_type = "session",
    object_id = TEST_MISSING_ID,
    file_size = 1024L,
    vb = FALSE
  ))
})
