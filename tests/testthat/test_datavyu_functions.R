library(databraryr)

# dv_to_csv ---------------------------------------------------
# test_that("dv_to_csv rejects bad input parameters", {
#   expect_error(dv_to_csv(dv_dir = -1))
#   expect_error(dv_to_csv(dv_dir = 0))
#   expect_error(dv_to_csv(dv_dir = list(a=1, b=2)))
#   expect_error(dv_to_csv(dv_dir = TRUE))
#
#   expect_error(dv_to_csv(dv_fn = -1))
#   expect_error(dv_to_csv(dv_fn = 0))
#   expect_error(dv_to_csv(dv_fn = list(a=1, b=2)))
#   expect_error(dv_to_csv(dv_fn = TRUE))
#
#   expect_error(dv_to_csv(out_fn = -1))
#   expect_error(dv_to_csv(out_fn = 0))
#   expect_error(dv_to_csv(out_fn = list(a=1, b=2)))
#   expect_error(dv_to_csv(out_fn = TRUE))
#
#   expect_error(dv_to_csv(auto_write_over = -1))
#   expect_error(dv_to_csv(auto_write_over = 3))
#   expect_error(dv_to_csv(auto_write_over = "a"))
#   expect_error(dv_to_csv(auto_write_over = list(a=1, b=2)))
#
#   expect_error(dv_to_csv(code_regex = -1))
#   expect_error(dv_to_csv(code_regex = 0))
#   expect_error(dv_to_csv(code_regex = list(a=1, b=2)))
#   expect_error(dv_to_csv(code_regex = TRUE))
#
#   expect_error(dv_to_csv(code_type_regex = -1))
#   expect_error(dv_to_csv(code_type_regex = 0))
#   expect_error(dv_to_csv(code_type_regex = list(a=1, b=2)))
#   expect_error(dv_to_csv(code_type_regex = TRUE))
#
#   expect_error(dv_to_csv(onset_offset_regex = -1))
#   expect_error(dv_to_csv(onset_offset_regex = 0))
#   expect_error(dv_to_csv(onset_offset_regex = list(a=1, b=2)))
#   expect_error(dv_to_csv(onset_offset_regex = TRUE))
#
#   expect_error(dv_to_csv(code_values_regex = -1))
#   expect_error(dv_to_csv(code_values_regex = 0))
#   expect_error(dv_to_csv(code_values_regex = list(a=1, b=2)))
#   expect_error(dv_to_csv(code_values_regex = TRUE))
#
#   expect_error(dv_to_csv(vb = -1))
#   expect_error(dv_to_csv(vb = 3))
#   expect_error(dv_to_csv(vb = "a"))
#   expect_error(dv_to_csv(vb = list(a=1, b=2)))
# })

# extract_dv ---------------------------------------------------
test_that("extract_dv rejects bad input parameters", {
  expect_error(extract_dv(in_dir = -1))
  expect_error(extract_dv(in_dir = 0))
  expect_error(extract_dv(in_dir = list(a=1, b=2)))
  expect_error(extract_dv(in_dir = TRUE))

  expect_error(extract_dv(in_fn = -1))
  expect_error(extract_dv(in_fn = 0))
  expect_error(extract_dv(in_fn = list(a=1, b=2)))
  expect_error(extract_dv(in_fn = TRUE))

  expect_error(extract_dv(out_dir = -1))
  expect_error(extract_dv(out_dir = 0))
  expect_error(extract_dv(out_dir = list(a=1, b=2)))
  expect_error(extract_dv(out_dir = TRUE))

  expect_error(extract_dv(auto_write_over = -1))
  expect_error(extract_dv(auto_write_over = 3))
  expect_error(extract_dv(auto_write_over = "a"))
  expect_error(extract_dv(auto_write_over = list(a=1, b=2)))

  expect_error(extract_dv(vb = -1))
  expect_error(extract_dv(vb = 3))
  expect_error(extract_dv(vb = "a"))
  expect_error(extract_dv(vb = list(a=1, b=2)))
})

# extract_dv_code_defs ---------------------------------------------------
test_that("extract_dv_code_defs rejects bad input parameters", {
  expect_error(extract_dv_code_defs(in_dir = -1))
  expect_error(extract_dv_code_defs(in_dir = 0))
#  expect_error(extract_dv_code_defs(in_dir = list(a=1, b=2)))
  expect_error(extract_dv_code_defs(in_dir = TRUE))

  expect_error(extract_dv_code_defs(dv_fn = -1))
  expect_error(extract_dv_code_defs(dv_fn = 0))
#  expect_error(extract_dv_code_defs(dv_fn = list(a=1, b=2)))
  expect_error(extract_dv_code_defs(dv_fn = TRUE))

  expect_error(extract_dv_code_defs(out_dir = -1))
  expect_error(extract_dv_code_defs(out_dir = 0))
#  expect_error(extract_dv_code_defs(out_dir = list(a=1, b=2)))
  expect_error(extract_dv_code_defs(out_dir = TRUE))

  expect_error(extract_dv_code_defs(out_fn = -1))
  expect_error(extract_dv_code_defs(out_fn = 0))
#  expect_error(extract_dv_code_defs(out_fn = list(a=1, b=2)))
  expect_error(extract_dv_code_defs(out_fn = TRUE))

  expect_error(extract_dv_code_defs(auto_write_over = -1))
  expect_error(extract_dv_code_defs(auto_write_over = 3))
  expect_error(extract_dv_code_defs(auto_write_over = "a"))
#  expect_error(extract_dv_code_defs(auto_write_over = list(a=1, b=2)))

  expect_error(extract_dv_code_defs(code_regex = -1))
  expect_error(extract_dv_code_defs(code_regex = 0))
#  expect_error(extract_dv_code_defs(code_regex = list(a=1, b=2)))
  expect_error(extract_dv_code_defs(code_regex = TRUE))

  expect_error(extract_dv_code_defs(code_vals_regex = -1))
  expect_error(extract_dv_code_defs(code_vals_regex = 0))
#  expect_error(extract_dv_code_defs(code_vals_regex = list(a=1, b=2)))
  expect_error(extract_dv_code_defs(code_vals_regex = TRUE))

  expect_error(extract_dv_code_defs(code_type_regex = -1))
  expect_error(extract_dv_code_defs(code_type_regex = 0))
#  expect_error(extract_dv_code_defs(code_type_regex = list(a=1, b=2)))
  expect_error(extract_dv_code_defs(code_type_regex = TRUE))

  expect_error(extract_dv_code_defs(vb = -1))
  expect_error(extract_dv_code_defs(vb = 3))
  expect_error(extract_dv_code_defs(vb = "a"))
#  expect_error(extract_dv_code_defs(vb = list(a=1, b=2)))
})
