# bulk_create_sessions() ------------------------------------------------------
login_test_account()

test_that("bulk_create_sessions rejects invalid args", {
  expect_error(bulk_create_sessions(vol_id = -1, session_names = "a"))
  expect_error(bulk_create_sessions(vol_id = 1, session_names = character(0)))
  expect_error(bulk_create_sessions(vol_id = 1, session_names = c("a", "a")))
  expect_error(bulk_create_sessions(
    vol_id = 1, session_names = c("a", "b"), source_date = rep("2024-01-01", 3)
  ))
})

test_that("bulk_create_sessions creates and deletes multiple sessions", {
  uniq <- sprintf(
    "bulk_cs_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  names_vec <- c(
    sprintf("%s_1", uniq), sprintf("%s_2", uniq), sprintf("%s_3", uniq)
  )

  result <- bulk_create_sessions(
    vol_id = TEST_VOL_ID,
    session_names = names_vec,
    vb = FALSE
  )
  skip_if_null_response(result, "bulk_create_sessions live API")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$input, names_vec)
  expect_true(all(result$status == "success"))

  ids <- vapply(result$result, function(r) {
    if (is.null(r) || is.null(r$id)) NA_real_ else as.numeric(r$id)
  }, numeric(1))
  on.exit({
    for (id in ids) {
      if (!is.na(id)) {
        try(
          delete_session(vol_id = TEST_VOL_ID, session_id = as.integer(id), vb = FALSE),
          silent = TRUE
        )
      }
    }
  }, add = TRUE)

  expect_false(any(is.na(ids)))
})
