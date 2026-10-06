# bulk_delete_files() ---------------------------------------------------------
login_test_account()

# Argument validation ---------------------------------------------------------

test_that("bulk_delete_files rejects invalid args", {
  expect_error(bulk_delete_files(
    vol_id = -1, session_id = 1, file_ids = 1
  ))
  expect_error(bulk_delete_files(
    vol_id = 1, session_id = 0, file_ids = 1
  ))
  expect_error(bulk_delete_files(
    vol_id = 1, session_id = 1, file_ids = numeric(0)
  ))
  expect_error(bulk_delete_files(
    vol_id = 1, session_id = 1, file_ids = c(1, -2)
  ))
  expect_error(bulk_delete_files(
    vol_id = 1, session_id = 1, file_ids = c(1, NA)
  ))
  expect_error(bulk_delete_files(
    vol_id = 1, session_id = 1, file_ids = "1"
  ))
})

# End-to-end ------------------------------------------------------------------

test_that("bulk_delete_files fast-fails on non-existent file ids", {
  session <- create_session(
    vol_id = TEST_VOL_ID, name = "bulk_delete_files fail test", vb = FALSE
  )
  skip_if_null_response(session, "create_session for bulk_delete_files")
  on.exit(
    delete_session(vol_id = TEST_VOL_ID, session_id = session$id, vb = FALSE),
    add = TRUE
  )

  partial <- tryCatch(
    bulk_delete_files(
      vol_id = TEST_VOL_ID,
      session_id = session$id,
      file_ids = c(999999999, 999999998),
      vb = FALSE
    ),
    databraryr_bulk_error = function(e) e$partial
  )

  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[1], "failed")
  expect_equal(partial$status[2], "pending")
})
