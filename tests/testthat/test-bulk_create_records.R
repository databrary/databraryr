# bulk_create_records() -------------------------------------------------------
login_test_account()

test_that("bulk_create_records rejects invalid args", {
  expect_error(bulk_create_records(vol_id = -1, record_names = "a", category_id = 1L))
  expect_error(bulk_create_records(vol_id = 1, record_names = character(0), category_id = 1L))
  expect_error(bulk_create_records(vol_id = 1, record_names = c("a", "a"), category_id = 6L))
  expect_error(bulk_create_records(
    vol_id = 1, record_names = c("a", "b"), category_id = rep(TEST_CATEGORY_ID, 3L)
  ))
})

test_that("bulk_create_records creates then deletes multiple records", {
  uniq <- sprintf(
    "bulk_cr_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  names_vec <- c(
    sprintf("%s_1", uniq), sprintf("%s_2", uniq), sprintf("%s_3", uniq)
  )

  result <- bulk_create_records(
    vol_id = TEST_VOL_ID,
    record_names = names_vec,
    category_id = TEST_CATEGORY_ID,
    vb = FALSE
  )
  skip_if_null_response(result, "bulk_create_records live API")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$input, names_vec)
  expect_true(all(result$status == "success"))

  ids <- vapply(result$result, function(r) {
    if (is.null(r) || is.null(r$record_id)) NA_real_ else as.numeric(r$record_id)
  }, numeric(1))
  on.exit({
    for (id in ids) {
      if (!is.na(id)) {
        try(
          delete_volume_record(vol_id = TEST_VOL_ID, record_id = as.integer(id), vb = FALSE),
          silent = TRUE
        )
      }
    }
  }, add = TRUE)

  expect_false(any(is.na(ids)))
})
