# bulk_unassign_records() -----------------------------------------------------
login_test_account()

test_that("bulk_unassign_records rejects invalid args", {
  expect_error(bulk_unassign_records(vol_id = -1, session_id = 1L, record_ids = 1L))
  expect_error(bulk_unassign_records(vol_id = 1L, session_id = -1L, record_ids = 1L))
  expect_error(bulk_unassign_records(vol_id = 1L, session_id = 1L, record_ids = numeric(0)))
})

test_that("bulk_unassign_records removes defaults for multiple records", {
  uniq <- sprintf(
    "bulk_ur_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  sid <- make_test_session(sprintf("%s_sess", uniq), vb = FALSE)
  skip_if_null_response(sid, "make_test_session")

  r1 <- make_test_record(sprintf("%s_r1", uniq), vb = FALSE)
  r2 <- make_test_record(sprintf("%s_r2", uniq), vb = FALSE)
  skip_if_null_response(r1, "make_test_record 1")
  skip_if_null_response(r2, "make_test_record 2")

  asg <- bulk_assign_records(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    record_ids = c(r1, r2),
    vb = FALSE
  )
  expect_true(all(asg$status == "success"))

  result <- bulk_unassign_records(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    record_ids = c(r1, r2),
    vb = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(result$status == "success"))
})

test_that("bulk_unassign_records fast-fails when record is not a default", {
  sid <- make_test_session("bulk_unassign_records ff", vb = FALSE)
  skip_if_null_response(sid, "make_test_session")

  r1 <- make_test_record("bulk_unassign_records ff r", vb = FALSE)
  skip_if_null_response(r1, "make_test_record")

  add_default_record_to_session(TEST_VOL_ID, sid, r1, vb = FALSE)

  mixed <- c(r1, TEST_MISSING_ID)

  partial <- tryCatch(
    bulk_unassign_records(
      vol_id = TEST_VOL_ID,
      session_id = sid,
      record_ids = mixed,
      vb = FALSE
    ),
    databraryr_bulk_error = function(e) e$partial
  )

  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[[1]], "success")
  expect_equal(partial$status[[2]], "failed")
})

test_that("bulk_unassign_records on_error collect runs all rows", {
  sid <- make_test_session("bulk_unassign_records collect", vb = FALSE)
  skip_if_null_response(sid, "make_test_session")

  created <- bulk_create_records(
    vol_id = TEST_VOL_ID,
    record_names = c("bulk_ur_c1", "bulk_ur_c2"),
    category_id = TEST_CATEGORY_ID,
    vb = FALSE
  )
  skip_if_null_response(created, "bulk_create_records")

  ids <- vapply(created$result, function(r) as.integer(r$record_id), integer(1))
  on.exit({
    for (id in ids) {
      try(delete_volume_record(TEST_VOL_ID, id, vb = FALSE), silent = TRUE)
    }
  }, add = TRUE)

  bulk_assign_records(TEST_VOL_ID, sid, ids, vb = FALSE)

  mixed <- c(ids[[1]], TEST_MISSING_ID, ids[[2]])
  result <- bulk_unassign_records(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    record_ids = mixed,
    vb = FALSE,
    on_error = "collect"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, c("success", "failed", "success"))
})
