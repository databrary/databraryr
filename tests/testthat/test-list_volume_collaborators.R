# list_volume_collaborators ---------------------------------------------------

test_that("list_volume_collaborators returns tibble", {
  login_test_account()
  result <- list_volume_collaborators(vol_id = 1)
  skip_if_null_response(result, "list_volume_collaborators(vol_id = 1)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("collaborator_id", "collaborator_user_id") %in% names(result)))
})

test_that("list_volume_collaborators rejects bad input parameters", {
  expect_error(list_volume_collaborators(vol_id = "a"))
  expect_error(list_volume_collaborators(vol_id = c(1, 2)))
  expect_error(list_volume_collaborators(vol_id = -1))
  expect_error(list_volume_collaborators(vb = "yes"))
})


