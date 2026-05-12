# upload_file() ----------------------------------------------------------------
login_test_account()

TEST_VOL <- 1777

test_that("upload_file rejects missing or empty path", {
  expect_error(upload_file(
    path = "/nope/does/not/exist", destination_type = "session", object_id = 1
  ))
  expect_error(upload_file(
    path = "", destination_type = "session", object_id = 1
  ))
})

test_that("upload_file rejects empty file", {
  empty <- tempfile()
  file.create(empty)
  on.exit(unlink(empty), add = TRUE)
  expect_error(upload_file(
    path = empty, destination_type = "session", object_id = 1
  ))
})

test_that("upload_file uploads a small file end-to-end", {
  session <- create_session(
    vol_id = TEST_VOL, name = "upload_file test", vb = FALSE
  )
  skip_if_null_response(session, "create_session for upload_file")
  on.exit(
    delete_session(vol_id = TEST_VOL, session_id = session$id, vb = FALSE),
    add = TRUE
  )

  tmp <- tempfile(fileext = ".bin")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(as.raw(seq_len(1024L) %% 256L), tmp)

  result <- upload_file(
    path = tmp,
    destination_type = "session",
    object_id = session$id,
    content_type = "application/octet-stream",
    vb = FALSE
  )
  skip_if_null_response(result, "upload_file end-to-end")

  expect_type(result, "list")
  expect_true(!is.null(result$status_url))
  expect_true(result$upload_type %in% c("single", "multipart"))
})

test_that("upload_file infers filename and content_type", {
  session <- create_session(
    vol_id = TEST_VOL, name = "upload_file inference test", vb = FALSE
  )
  skip_if_null_response(session, "create_session for upload_file inference")
  on.exit(
    delete_session(vol_id = TEST_VOL, session_id = session$id, vb = FALSE),
    add = TRUE
  )

  tmp <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(as.raw(seq_len(1024L) %% 256L), tmp)

  result <- upload_file(
    path = tmp,
    destination_type = "session",
    object_id = session$id,
    vb = FALSE
  )
  skip_if_null_response(result, "upload_file inference")

  expect_type(result, "list")
})

# Internal helpers -------------------------------------------------------------

test_that("read_file_chunk reads a byte range", {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeBin(as.raw(0:255), tmp)

  expect_equal(databraryr:::read_file_chunk(tmp, 0, 4), as.raw(0:3))
  expect_equal(databraryr:::read_file_chunk(tmp, 10, 3), as.raw(10:12))
  # Past EOF returns whatever remains.
  expect_equal(
    length(databraryr:::read_file_chunk(tmp, 250, 100)),
    6L
  )
})

test_that("unquote_etag strips surrounding quotes", {
  expect_equal(databraryr:::unquote_etag('"abc123"'), "abc123")
  expect_equal(databraryr:::unquote_etag("abc123"), "abc123")
  expect_equal(databraryr:::unquote_etag(""), "")
  expect_null(databraryr:::unquote_etag(NULL))
})

test_that("normalize_part_urls sorts by part_number", {
  shuffled <- list(
    list(part_number = 3L, url = "c"),
    list(part_number = 1L, url = "a"),
    list(part_number = 2L, url = "b")
  )
  sorted <- databraryr:::normalize_part_urls(shuffled)
  expect_equal(
    vapply(sorted, function(p) p$url, character(1)),
    c("a", "b", "c")
  )
})

test_that("guess_content_type maps common extensions", {
  expect_equal(databraryr:::guess_content_type("a.mp4"), "video/mp4")
  expect_equal(databraryr:::guess_content_type("a.MP4"), "video/mp4")
  expect_equal(databraryr:::guess_content_type("a.csv"), "text/csv")
  expect_equal(
    databraryr:::guess_content_type("a.unknown"),
    "application/octet-stream"
  )
  expect_equal(
    databraryr:::guess_content_type("noext"),
    "application/octet-stream"
  )
})
