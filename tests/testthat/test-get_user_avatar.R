# get_user_avatar() ---------------------------------------------------------
login_test_account()

test_that("get_user_avatar returns raw bytes when dest_path is NULL", {
  # User ID 5 is known to have an avatar
  result <- get_user_avatar(user_id = 5, vb = FALSE)
  skip_if_null_response(result, "get_user_avatar(user_id = 5)")

  expect_type(result, "raw")
  expect_gt(length(result), 0)
})

test_that("get_user_avatar saves to file when dest_path is provided", {
  # User ID 5 is known to have an avatar
  temp_file <- tempfile(fileext = ".jpg")

  result <- get_user_avatar(
    user_id = 5,
    dest_path = temp_file,
    vb = FALSE
  )
  skip_if_null_response(result, "get_user_avatar(user_id = 5, dest_path = temp_file)")

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_gt(file.size(result), 0)

  # Clean up
  unlink(result)
})

test_that("get_user_avatar creates parent directories if needed", {
  # User ID 5 is known to have an avatar
  temp_dir <- tempfile()
  nested_path <- file.path(temp_dir, "subdir", "avatar.jpg")

  result <- get_user_avatar(
    user_id = 5,
    dest_path = nested_path,
    vb = FALSE
  )
  skip_if_null_response(result, "get_user_avatar(user_id = 5, dest_path = nested_path)")

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_gt(file.size(result), 0)

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("get_user_avatar returns NULL for non-existent user", {
  result <- get_user_avatar(user_id = TEST_MISSING_ID, vb = FALSE)
  expect_null(result)
})

test_that("get_user_avatar works with verbose mode", {
  # User ID 5 is known to have an avatar
  result <- get_user_avatar(user_id = 5, vb = TRUE)
  skip_if_null_response(result, "get_user_avatar(user_id = 5, vb = TRUE)")

  expect_type(result, "raw")
  expect_gt(length(result), 0)
})

test_that("get_user_avatar rejects invalid user_id", {
  # Non-numeric user_id
  expect_error(get_user_avatar(user_id = "abc"))
  expect_error(get_user_avatar(user_id = TRUE))
  expect_error(get_user_avatar(user_id = list(a = 1)))

  # Multiple values
  expect_error(get_user_avatar(user_id = c(1, 2)))

  # Negative or zero user_id
  expect_error(get_user_avatar(user_id = 0))
  expect_error(get_user_avatar(user_id = -1))

  # NULL or NA user_id
  expect_error(get_user_avatar(user_id = NULL))
  expect_error(get_user_avatar(user_id = NA))
})

test_that("get_user_avatar rejects invalid dest_path", {
  # Non-character dest_path
  expect_error(get_user_avatar(user_id = 5, dest_path = 123))
  expect_error(get_user_avatar(user_id = 5, dest_path = TRUE))
  expect_error(get_user_avatar(user_id = 5, dest_path = list(a = 1)))

  # Multiple values
  expect_error(get_user_avatar(user_id = 5, dest_path = c("path1", "path2")))
})

test_that("get_user_avatar rejects invalid vb parameter", {
  expect_error(get_user_avatar(user_id = 5, vb = -1))
  expect_error(get_user_avatar(user_id = 5, vb = 3))
  expect_error(get_user_avatar(user_id = 5, vb = "a"))
  expect_error(get_user_avatar(user_id = 5, vb = list(a = 1, b = 2)))
  expect_error(get_user_avatar(user_id = 5, vb = c(TRUE, FALSE)))
  expect_error(get_user_avatar(user_id = 5, vb = NULL))
})

test_that("get_user_avatar rejects invalid rq parameter", {
  expect_error(get_user_avatar(user_id = 5, rq = "a"))
  expect_error(get_user_avatar(user_id = 5, rq = -1))
  expect_error(get_user_avatar(user_id = 5, rq = c(2, 3)))
  expect_error(get_user_avatar(user_id = 5, rq = list(a = 1, b = 2)))
  expect_error(get_user_avatar(user_id = 5, rq = TRUE))
})

test_that("get_user_avatar bytes and file content are identical", {
  # User ID 5 is known to have an avatar
  # Get bytes
  bytes_result <- get_user_avatar(user_id = 5, vb = FALSE)
  skip_if_null_response(bytes_result, "get_user_avatar(user_id = 5)")

  # Save to file
  temp_file <- tempfile(fileext = ".jpg")
  file_result <- get_user_avatar(
    user_id = 5,
    dest_path = temp_file,
    vb = FALSE
  )
  skip_if_null_response(file_result, "get_user_avatar(user_id = 5, dest_path = temp_file)")

  # Read file and compare
  file_bytes <- readBin(file_result, "raw", file.info(file_result)$size)
  expect_equal(bytes_result, file_bytes)

  # Clean up
  unlink(file_result)
})

test_that("get_user_avatar saves to directory with auto-determined filename", {
  # User ID 5 is known to have an avatar
  # Create a temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir)

  result <- get_user_avatar(
    user_id = 5,
    dest_path = temp_dir,
    vb = FALSE
  )
  skip_if_null_response(result, "get_user_avatar(user_id = 5, dest_path = temp_dir)")

  expect_type(result, "character")
  expect_true(file.exists(result))
  expect_gt(file.size(result), 0)

  # Check that the file is in the temp_dir
  expect_true(startsWith(result, normalizePath(temp_dir)))

  # Check that filename was auto-determined
  filename <- basename(result)
  expect_true(nchar(filename) > 0)
  # The filename should either be from content-disposition header or our fallback
  # Accept any reasonable filename pattern
  expect_true(grepl("avatar|user", filename, ignore.case = TRUE) || filename == "downloaded_file")

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("get_user_avatar works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- get_user_avatar(user_id = 5, rq = custom_rq, vb = FALSE)
  skip_if_null_response(result, "get_user_avatar(user_id = 5, rq = custom_rq)")

  expect_type(result, "raw")
  expect_gt(length(result), 0)
})

test_that("get_user_avatar handles overwriting existing files", {
  # User ID 5 is known to have an avatar
  temp_file <- tempfile(fileext = ".jpg")

  # First write
  result1 <- get_user_avatar(
    user_id = 5,
    dest_path = temp_file,
    vb = FALSE
  )
  skip_if_null_response(result1, "get_user_avatar(user_id = 5, dest_path = temp_file)")

  first_size <- file.size(result1)

  # Second write (overwrite)
  result2 <- get_user_avatar(
    user_id = 5,
    dest_path = temp_file,
    vb = FALSE
  )
  skip_if_null_response(result2, "get_user_avatar(user_id = 5, dest_path = temp_file) [overwrite]")

  second_size <- file.size(result2)

  # Both should point to same file
  expect_equal(result1, result2)
  # Sizes should be the same (same avatar)
  expect_equal(first_size, second_size)

  # Clean up
  unlink(result2)
})