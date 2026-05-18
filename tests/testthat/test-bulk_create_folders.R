# bulk_create_folders() -------------------------------------------------------
login_test_account()

test_that("bulk_create_folders rejects invalid args", {
  expect_error(bulk_create_folders(vol_id = -1, folder_names = "a"))
  expect_error(bulk_create_folders(vol_id = 1, folder_names = character(0)))
  expect_error(bulk_create_folders(vol_id = 1, folder_names = c("a", "a")))
  expect_error(bulk_create_folders(
    vol_id = 1, folder_names = c("a", "b"), release_level = c("X", "Y", "Z")
  ))
})

test_that("bulk_create_folders creates and deletes multiple folders", {
  uniq <- sprintf(
    "bulk_cf_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  names_vec <- c(
    sprintf("%s_1", uniq), sprintf("%s_2", uniq), sprintf("%s_3", uniq)
  )

  result <- bulk_create_folders(
    vol_id = TEST_VOL_ID,
    folder_names = names_vec,
    vb = FALSE
  )
  skip_if_null_response(result, "bulk_create_folders live API")

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
          delete_folder(vol_id = TEST_VOL_ID, folder_id = as.integer(id), vb = FALSE),
          silent = TRUE
        )
      }
    }
  }, add = TRUE)

  expect_false(any(is.na(ids)))
})
