# get_permission_levels -------------------------------------------------------
test_that("get_permission_levels returns a character array", {
  login_test_account()
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
  login_test_account()
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
