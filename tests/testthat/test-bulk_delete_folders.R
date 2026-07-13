# bulk_delete_folders() -------------------------------------------------------
login_test_account()

# Argument validation ---------------------------------------------------------

test_that("bulk_delete_folders rejects invalid args", {
  expect_error(bulk_delete_folders(vol_id = -1, folder_ids = 1))
  expect_error(bulk_delete_folders(vol_id = 1, folder_ids = numeric(0)))
  expect_error(bulk_delete_folders(vol_id = 1, folder_ids = c(1, -2)))
  expect_error(bulk_delete_folders(vol_id = 1, folder_ids = c(1, NA)))
  expect_error(bulk_delete_folders(vol_id = 1, folder_ids = c(1.5, 2)))
  expect_error(bulk_delete_folders(vol_id = 1, folder_ids = "1"))
})

# End-to-end ------------------------------------------------------------------

test_that("bulk_delete_folders deletes multiple folders", {
  ids <- vapply(seq_len(3), function(i) {
    f <- create_folder(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_delete_folders test %d", i),
      vb = FALSE
    )
    if (is.null(f)) NA_real_ else as.numeric(f$id)
  }, numeric(1))

  if (any(is.na(ids))) {
    testthat::skip("Could not create test folders on staging.")
  }

  result <- bulk_delete_folders(
    vol_id = TEST_VOL_ID, folder_ids = ids, vb = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true(all(result$status == "success"))
})

test_that("bulk_delete_folders fast-fails on non-existent folder", {
  partial <- tryCatch(
    bulk_delete_folders(
      vol_id = TEST_VOL_ID,
      folder_ids = c(999999999, 999999998),
      vb = FALSE
    ),
    databraryr_bulk_error = function(e) e$partial
  )

  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[1], "failed")
  expect_equal(partial$status[2], "pending")
})
