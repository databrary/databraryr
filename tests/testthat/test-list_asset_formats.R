test_that("list_asset_formats returns format metadata", {
  login_test_account()
  formats <- suppressWarnings(list_asset_formats())
  skip_if_null_response(formats, "list_asset_formats()")
  expect_true(is.data.frame(formats))
  expect_true(all(c("format_id", "format_mimetype", "format_name", "category") %in% names(formats)))
  expect_gt(nrow(formats), 0)
})

test_that("list_asset_formats rejects bad input parameters", {
  expect_error(list_asset_formats(vb = -1))
  expect_error(list_asset_formats(vb = 2))
  expect_error(list_asset_formats(vb = "a"))
})

