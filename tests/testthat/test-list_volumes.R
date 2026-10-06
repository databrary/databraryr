# list_volumes ----------------------------------------------------------------

test_that("list_volumes returns tibble", {
  login_test_account()
  result <- list_volumes(search = "workshop")
  skip_if_null_response(result, "list_volumes(search = 'workshop')")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("volume_id", "volume_title") %in% names(result)))
})

test_that("list_volumes rejects bad input parameters", {
  expect_error(list_volumes(search = 123))
  expect_error(list_volumes(ordering = TRUE))
  expect_error(list_volumes(vb = "yes"))
})


