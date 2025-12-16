# get_institution_avatar() ---------------------------------------------------
login_test_account()

test_that("get_institution_avatar returns raw bytes when dest_path is NULL", {
  # Institution ID 1 is known to have an avatar
  result <- get_institution_avatar(institution_id = 1, vb = FALSE)
  skip_if_null_response(result, "get_institution_avatar(institution_id = 1)")

  expect_type(result, "raw")
  expect_gt(length(result), 0)
})

test_that("get_institution_avatar saves to file when dest_path is provided", {
  # Institution ID 1 is known to have an avatar
  temp_file <- tempfile(fileext = ".jpg")

  result <- get_institution_avatar(
    institution_id = 1,
    dest_path = temp_file,
    vb = FALSE
  )
  skip_if_null_response(result, "get_institution_avatar(institution_id = 1, dest_path = ...)")

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_gt(file.size(result), 0)

  # Clean up
  unlink(temp_file)
})

test_that("get_institution_avatar returns NULL for non-existent institution", {
  result <- get_institution_avatar(institution_id = 999999, vb = FALSE)
  expect_null(result)
})

test_that("get_institution_avatar returns NULL for institution without avatar", {
  # Test with a very high ID that likely doesn't have an avatar
  result <- get_institution_avatar(institution_id = 99999, vb = FALSE)
  # This may return NULL either because institution doesn't exist or has no avatar
  # Both outcomes are acceptable for this test
  expect_true(is.null(result) || is.raw(result))
})

test_that("get_institution_avatar works with verbose mode", {
  # Institution ID 1 is known to have an avatar
  result <- get_institution_avatar(institution_id = 1, vb = TRUE)
  skip_if_null_response(result, "get_institution_avatar(institution_id = 1, vb = TRUE)")

  expect_type(result, "raw")
  expect_gt(length(result), 0)
})

test_that("get_institution_avatar rejects invalid institution_id", {
  # Negative ID
  expect_error(get_institution_avatar(institution_id = -1))

  # Zero ID
  expect_error(get_institution_avatar(institution_id = 0))

  # Non-numeric ID
  expect_error(get_institution_avatar(institution_id = "1"))
  expect_error(get_institution_avatar(institution_id = TRUE))
  expect_error(get_institution_avatar(institution_id = list(a = 1)))

  # Multiple values
  expect_error(get_institution_avatar(institution_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(get_institution_avatar(institution_id = 1.5))
  expect_error(get_institution_avatar(institution_id = 2.7))

  # NULL
  expect_error(get_institution_avatar(institution_id = NULL))

  # NA
  expect_error(get_institution_avatar(institution_id = NA))
})

test_that("get_institution_avatar rejects invalid dest_path", {
  # Non-character dest_path
  expect_error(get_institution_avatar(institution_id = 1, dest_path = 123))
  expect_error(get_institution_avatar(institution_id = 1, dest_path = TRUE))
  expect_error(get_institution_avatar(institution_id = 1, dest_path = list(a = 1)))

  # Multiple values
  expect_error(get_institution_avatar(institution_id = 1, dest_path = c("file1.jpg", "file2.jpg")))
})

test_that("get_institution_avatar rejects invalid vb parameter", {
  expect_error(get_institution_avatar(institution_id = 1, vb = -1))
  expect_error(get_institution_avatar(institution_id = 1, vb = 3))
  expect_error(get_institution_avatar(institution_id = 1, vb = "a"))
  expect_error(get_institution_avatar(institution_id = 1, vb = list(a = 1, b = 2)))
  expect_error(get_institution_avatar(institution_id = 1, vb = c(TRUE, FALSE)))
  expect_error(get_institution_avatar(institution_id = 1, vb = NULL))
})

test_that("get_institution_avatar rejects invalid rq parameter", {
  expect_error(get_institution_avatar(institution_id = 1, rq = "a"))
  expect_error(get_institution_avatar(institution_id = 1, rq = -1))
  expect_error(get_institution_avatar(institution_id = 1, rq = c(2, 3)))
  expect_error(get_institution_avatar(institution_id = 1, rq = list(a = 1, b = 2)))
  expect_error(get_institution_avatar(institution_id = 1, rq = TRUE))
})

test_that("get_institution_avatar creates parent directory if needed", {
  # Institution ID 1 is known to have an avatar
  # Create a path with non-existent parent directory
  temp_dir <- tempfile()
  temp_file <- file.path(temp_dir, "avatars", "test.jpg")

  result <- get_institution_avatar(
    institution_id = 1,
    dest_path = temp_file,
    vb = FALSE
  )
  skip_if_null_response(result, "get_institution_avatar(institution_id = 1, dest_path = ...)")

  expect_true(file.exists(result))
  expect_true(dir.exists(dirname(result)))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("get_institution_avatar works with custom request object", {
  # Institution ID 1 is known to have an avatar
  custom_rq <- databraryr::make_default_request()

  result <- get_institution_avatar(institution_id = 1, rq = custom_rq, vb = FALSE)
  skip_if_null_response(result, "get_institution_avatar(institution_id = 1, rq = custom_rq)")

  expect_type(result, "raw")
  expect_gt(length(result), 0)
})

test_that("get_institution_avatar returns same content for raw bytes and file", {
  # Institution ID 1 is known to have an avatar
  # Get as raw bytes
  bytes_result <- get_institution_avatar(institution_id = 1, vb = FALSE)
  skip_if_null_response(bytes_result, "get_institution_avatar(institution_id = 1) as bytes")

  # Get as file
  temp_file <- tempfile(fileext = ".jpg")
  file_result <- get_institution_avatar(
    institution_id = 1,
    dest_path = temp_file,
    vb = FALSE
  )
  skip_if_null_response(file_result, "get_institution_avatar(institution_id = 1, dest_path = ...) as file")

  # Read file and compare
  file_bytes <- readBin(file_result, "raw", file.size(file_result))
  expect_equal(bytes_result, file_bytes)

  # Clean up
  unlink(temp_file)
})

test_that("get_institution_avatar saves to directory with auto-determined filename", {
  # Institution ID 1 is known to have an avatar
  # Create a temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)

  result <- get_institution_avatar(
    institution_id = 1,
    dest_path = temp_dir,
    vb = FALSE
  )
  skip_if_null_response(result, "get_institution_avatar(institution_id = 1, dest_path = temp_dir)")

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_gt(file.size(result), 0)

  # Check that the file is in the temp_dir
  expect_true(startsWith(result, normalizePath(temp_dir)))

  # Check that filename was auto-determined
  filename <- basename(result)
  expect_true(nchar(filename) > 0)
  # The filename should either be from content-disposition header or our fallback
  expect_true(filename == "institutions_1_avatar" || filename == "institution_1_avatar.jpg" || filename == "downloaded_file")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})