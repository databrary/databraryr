# bulk_create_records() -------------------------------------------------------
login_test_account()

test_that("bulk_create_records rejects invalid args", {
  expect_error(bulk_create_records(vol_id = -1, record_names = "a", category_id = 1L))
  expect_error(bulk_create_records(vol_id = 1, record_names = character(0), category_id = 1L))
  expect_error(bulk_create_records(vol_id = 1, record_names = c("a", "a"), category_id = 6L))
  expect_error(bulk_create_records(
    vol_id = 1, record_names = c("a", "b"), category_id = rep(TEST_CATEGORY_ID, 3L)
  ))
  expect_error(bulk_create_records(
    vol_id = 1,
    record_names = c("a", "b"),
    category_id = TEST_CATEGORY_ID,
    measures = rep(list(list("30" = "x")), 3L)
  ))
  expect_error(bulk_create_records(
    vol_id = 1,
    record_names = c("a", "b"),
    category_id = TEST_CATEGORY_ID,
    measures = "not a list"
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

test_that("bulk_create_records recycles shared measures across rows", {
  uniq <- sprintf(
    "bulk_cr_meas_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  names_vec <- c(sprintf("%s_1", uniq), sprintf("%s_2", uniq))
  shared_value <- sprintf("shared_%s", uniq)

  result <- bulk_create_records(
    vol_id = TEST_VOL_ID,
    record_names = names_vec,
    category_id = TEST_CATEGORY_ID,
    measures = stats::setNames(list(shared_value), as.character(TEST_METRIC_ID_EXTRA)),
    vb = FALSE
  )
  skip_if_null_response(result, "bulk_create_records with recycled measures")

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

  extra_vals <- vapply(result$result, function(r) {
    if (is.null(r) || is.null(r$measures)) {
      NA_character_
    } else {
      as.character(r$measures[[as.character(TEST_METRIC_ID_EXTRA)]])
    }
  }, character(1))
  expect_true(all(extra_vals == shared_value))
})

test_that("bulk_create_records applies per-row measures", {
  uniq <- sprintf(
    "bulk_cr_pmeas_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  names_vec <- c(sprintf("%s_1", uniq), sprintf("%s_2", uniq))
  val1 <- sprintf("row1_%s", uniq)
  val2 <- sprintf("row2_%s", uniq)

  result <- bulk_create_records(
    vol_id = TEST_VOL_ID,
    record_names = names_vec,
    category_id = TEST_CATEGORY_ID,
    measures = list(
      stats::setNames(list(val1), as.character(TEST_METRIC_ID_EXTRA)),
      stats::setNames(list(val2), as.character(TEST_METRIC_ID_EXTRA))
    ),
    vb = FALSE
  )
  skip_if_null_response(result, "bulk_create_records with per-row measures")

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

  extra_vals <- vapply(result$result, function(r) {
    if (is.null(r) || is.null(r$measures)) {
      NA_character_
    } else {
      as.character(r$measures[[as.character(TEST_METRIC_ID_EXTRA)]])
    }
  }, character(1))
  expect_setequal(extra_vals, c(val1, val2))
})
