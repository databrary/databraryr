# get_file_duration ---------------------------------------------------------
test_that("get_file_duration returns duration metadata for a known asset", {
  login_test_account()
  result <- get_file_duration()
  skip_if_null_response(result, "get_file_duration()")
  expect_true(is.numeric(result) && length(result) == 1)

  asset_detail <- perform_api_get(
    path = sprintf(API_SESSION_FILE_DETAIL, 2, 9, 2),
    vb = FALSE
  )
  expect_true("thumbnail_url" %in% names(asset_detail))
  expect_true(is.null(asset_detail$thumbnail_url) || nzchar(asset_detail$thumbnail_url))
})

test_that("get_file_duration rejects bad input parameters", {
  expect_error(get_file_duration(vol_id = "a"))
  expect_error(get_file_duration(vol_id = -1))
  expect_error(get_file_duration(vol_id = c(1, 3)))

  expect_error(get_file_duration(session_id = "a"))
  expect_error(get_file_duration(session_id = -1))
  expect_error(get_file_duration(session_id = c(1, 3)))

  expect_error(get_file_duration(asset_id = "a"))
  expect_error(get_file_duration(asset_id = -1))
  expect_error(get_file_duration(asset_id = c(1, 3)))
  
  expect_error(get_file_duration(vb = "a"))
  expect_error(get_file_duration(vb = -1))
  expect_error(get_file_duration(vb = c(2, 3)))
})

# get_permission_levels -------------------------------------------------------
test_that("get_permission_levels returns a character array", {
  levels <- get_permission_levels()
  expect_true(is.character(levels))
  expect_true(length(levels) > 0)
})

test_that("get_permission_levels handles vb flag", {
  expect_silent(get_permission_levels(vb = TRUE))
  expect_silent(get_permission_levels(vb = FALSE))
})

# get_release_levels ---------------------------------------------------------
test_that("get_release_levels returns a character array", {
  levels <- get_release_levels()
  expect_true(is.character(levels))
  expect_true(length(levels) == 4)
})

test_that("get_release_levels handles vb flag", {
  expect_silent(get_release_levels(vb = TRUE))
  expect_silent(get_release_levels(vb = FALSE))
})

# get_supported_file_types ----------------------------------------------------

test_that("get_supported_file_types returns data.frame", {
  expect_true(is.data.frame(get_supported_file_types()))
})

test_that("get_supported_file_types rejects bad input parameters", {
  expect_error(get_supported_file_types(vb = -1))
  expect_error(get_supported_file_types(vb = 3))
  expect_error(get_supported_file_types(vb = "a"))
})

# HHMMSSmmm_to_ms ---------------------------------------------------
test_that("HHMMSSmmm_to_ms returns number", {
  expect_true(class(HHMMSSmmm_to_ms()) == "numeric")
})

test_that("HHMMSSmmm_to_ms rejects bad input parameters", {
  expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = -1))
  #expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = "a"))
  #expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = list(a=1, b=2)))
  expect_error(HHMMSSmmm_to_ms(HHMMSSmmm = TRUE))
})

# make_fn_portable ---------------------------------------------------
test_that("make_fn_portable returns string", {
  expect_true("character" %in% class(make_fn_portable("}*&!@#$%^+.pdf")))
})

test_that("make_fn_portable rejects bad input parameters", {
  expect_error(make_fn_portable(fn = -1))
  expect_error(make_fn_portable(fn = list(a = 1, b = 2)))
  expect_error(make_fn_portable(fn = TRUE))
  
  expect_error(make_fn_portable(vb = -1))
  expect_error(make_fn_portable(vb = list(a = 1, b = 2)))
  expect_error(make_fn_portable(vb = 'a'))
  
  expect_error(make_fn_portable(replace_regex = -1))
  expect_error(make_fn_portable(replace_regex = list(a = 1, b = 2)))
  expect_error(make_fn_portable(replace_regex = TRUE))
  
  expect_error(make_fn_portable(replacement_char = -1))
  expect_error(make_fn_portable(replacement_char = list(a = 1, b = 2)))
  expect_error(make_fn_portable(replacement_char = TRUE))
  
})
