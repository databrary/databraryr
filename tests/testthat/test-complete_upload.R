# complete_upload() ------------------------------------------------------------
login_test_account()

test_that("complete_upload validates required args", {
  expect_error(complete_upload(
    upload_guid = "", s3_upload_id = "x",
    parts = list(list(part_number = 1L, etag = "e"))
  ))
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "",
    parts = list(list(part_number = 1L, etag = "e"))
  ))
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "x", parts = list()
  ))
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "x",
    parts = list(list(part_number = 0L, etag = "e"))
  ))
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "x",
    parts = list(list(part_number = 1L, etag = ""))
  ))
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "x",
    parts = list(list(part_number = 1L))
  ))
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "x",
    parts = list(list(etag = "e"))
  ))
})

test_that("complete_upload returns NULL for unknown upload_guid", {
  res <- complete_upload(
    upload_guid = "00000000-0000-0000-0000-000000000000",
    s3_upload_id = "not-a-real-upload-id",
    parts = list(list(part_number = 1L, etag = "deadbeef")),
    vb = FALSE
  )
  expect_null(res)
})

test_that("complete_upload rejects invalid vb / rq", {
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "x",
    parts = list(list(part_number = 1L, etag = "e")),
    vb = "yes"
  ))
  expect_error(complete_upload(
    upload_guid = "g", s3_upload_id = "x",
    parts = list(list(part_number = 1L, etag = "e")),
    rq = "not-a-request"
  ))
})
