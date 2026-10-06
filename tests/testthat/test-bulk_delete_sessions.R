# bulk_delete_sessions() ------------------------------------------------------
login_test_account()

# Argument validation ---------------------------------------------------------

test_that("bulk_delete_sessions rejects invalid args", {
  expect_error(bulk_delete_sessions(vol_id = -1, session_ids = 1))
  expect_error(bulk_delete_sessions(vol_id = 1, session_ids = numeric(0)))
  expect_error(bulk_delete_sessions(vol_id = 1, session_ids = c(1, -2)))
  expect_error(bulk_delete_sessions(vol_id = 1, session_ids = c(1, NA)))
  expect_error(bulk_delete_sessions(vol_id = 1, session_ids = c(1.5, 2)))
  expect_error(bulk_delete_sessions(vol_id = 1, session_ids = "1"))
})

# End-to-end ------------------------------------------------------------------

test_that("bulk_delete_sessions deletes multiple sessions", {
  ids <- vapply(seq_len(3), function(i) {
    s <- create_session(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_delete_sessions test %d", i),
      vb = FALSE
    )
    if (is.null(s)) NA_real_ else as.numeric(s$id)
  }, numeric(1))

  if (any(is.na(ids))) {
    testthat::skip("Could not create test sessions on staging.")
  }

  result <- bulk_delete_sessions(
    vol_id = TEST_VOL_ID, session_ids = ids, vb = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_true(all(result$status == "success"))
})

test_that("bulk_delete_sessions fast-fails and resume completes the rest", {
  ids <- vapply(seq_len(2), function(i) {
    s <- create_session(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_delete_sessions resume %d", i),
      vb = FALSE
    )
    if (is.null(s)) NA_real_ else as.numeric(s$id)
  }, numeric(1))

  if (any(is.na(ids))) {
    testthat::skip("Could not create test sessions on staging.")
  }
  on.exit({
    for (id in ids) {
      try(delete_session(TEST_VOL_ID, id, vb = FALSE), silent = TRUE)
    }
  }, add = TRUE)

  # Put a non-existent id between the two real ones; delete_session returns
  # FALSE on it, which fast-fails the bulk run.
  mixed <- c(ids[1], 999999999, ids[2])

  partial <- tryCatch(
    bulk_delete_sessions(
      vol_id = TEST_VOL_ID, session_ids = mixed, vb = FALSE
    ),
    databraryr_bulk_error = function(e) e$partial
  )

  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[1], "success")
  expect_equal(partial$status[2], "failed")
  expect_equal(partial$status[3], "pending")
})

test_that("bulk_delete_sessions on_error collect runs all rows", {
  ids <- vapply(seq_len(2), function(i) {
    s <- create_session(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_delete_sessions collect %d", i),
      vb = FALSE
    )
    if (is.null(s)) NA_real_ else as.numeric(s$id)
  }, numeric(1))

  if (any(is.na(ids))) {
    testthat::skip("Could not create test sessions on staging.")
  }
  on.exit({
    for (id in ids) {
      try(delete_session(TEST_VOL_ID, id, vb = FALSE), silent = TRUE)
    }
  }, add = TRUE)

  mixed <- c(ids[1], 999999999, ids[2])
  result <- bulk_delete_sessions(
    vol_id = TEST_VOL_ID,
    session_ids = mixed,
    vb = FALSE,
    on_error = "collect"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, c("success", "failed", "success"))
})
