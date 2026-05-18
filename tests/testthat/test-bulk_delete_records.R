# bulk_delete_records() -------------------------------------------------------
login_test_account()

test_that("bulk_delete_records rejects invalid args", {
  expect_error(bulk_delete_records(vol_id = -1, record_ids = 1L))
  expect_error(bulk_delete_records(vol_id = 1, record_ids = numeric(0)))
  expect_error(bulk_delete_records(vol_id = 1, record_ids = c(1L, -2L)))
  expect_error(bulk_delete_records(vol_id = 1, record_ids = c(1L, NA)))
})

test_that("bulk_delete_records deletes multiple records", {
  uniq <- sprintf(
    "bulk_delrec_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  created <- bulk_create_records(
    vol_id = TEST_VOL_ID,
    record_names = c(sprintf("%s_1", uniq), sprintf("%s_2", uniq)),
    category_id = TEST_CATEGORY_ID,
    vb = FALSE
  )
  skip_if_null_response(created, "bulk_create_records for bulk_delete_records")

  ids <- vapply(created$result, function(r) as.numeric(r$record_id), numeric(1))

  result <- bulk_delete_records(vol_id = TEST_VOL_ID, record_ids = ids, vb = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(result$status == "success"))
})

test_that("bulk_delete_records fast-fails on missing record id", {
  uniq <- sprintf(
    "bulk_delrec_ff_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  one <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = sprintf("%s_single", uniq),
    vb = FALSE
  )
  skip_if_null_response(one, "create_volume_record")

  rid <- as.integer(one$record_id)
  on.exit(try(delete_volume_record(TEST_VOL_ID, rid, vb = FALSE), silent = TRUE), add = TRUE)

  mixed <- c(rid, TEST_MISSING_ID)

  partial <- tryCatch(
    bulk_delete_records(vol_id = TEST_VOL_ID, record_ids = mixed, vb = FALSE),
    databraryr_bulk_error = function(e) e$partial
  )

  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[[1]], "success")
  expect_equal(partial$status[[2]], "failed")
})

test_that("bulk_delete_records on_error collect runs all rows", {
  uniq <- sprintf(
    "bulk_delrec_coll_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  created <- bulk_create_records(
    vol_id = TEST_VOL_ID,
    record_names = c(sprintf("%s_1", uniq), sprintf("%s_2", uniq)),
    category_id = TEST_CATEGORY_ID,
    vb = FALSE
  )
  skip_if_null_response(created, "bulk_create_records")

  ids <- vapply(created$result, function(r) as.numeric(r$record_id), numeric(1))

  mixed <- c(ids[[1]], TEST_MISSING_ID, ids[[2]])
  result <- bulk_delete_records(
    vol_id = TEST_VOL_ID,
    record_ids = mixed,
    vb = FALSE,
    on_error = "collect"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, c("success", "failed", "success"))
})
