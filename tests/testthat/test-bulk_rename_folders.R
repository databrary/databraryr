# bulk_rename_folders() -------------------------------------------------------
login_test_account()

test_that("bulk_rename_folders rejects invalid args", {
  expect_error(bulk_rename_folders(vol_id = -1, folder_ids = 1L, new_names = "a"))
  expect_error(bulk_rename_folders(vol_id = 1, folder_ids = integer(0), new_names = character(0)))
  expect_error(bulk_rename_folders(vol_id = 1L, folder_ids = c(1L, 2L), new_names = "a"))
  expect_error(bulk_rename_folders(vol_id = 1L, folder_ids = c(1L, 1L), new_names = c("a", "b")))
})

test_that("bulk_rename_folders renames multiple folders", {
  uniq <- sprintf(
    "bulk_rf_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  fids <- integer(3)
  for (i in seq_along(fids)) {
    nm <- sprintf("%s_%d", uniq, i)
    fid <- make_test_folder(nm, vb = FALSE)
    skip_if_null_response(fid, "make_test_folder for bulk_rename_folders")
    fids[[i]] <- fid
  }

  new_names <- c(
    sprintf("%s_new_a", uniq),
    sprintf("%s_new_b", uniq),
    sprintf("%s_new_c", uniq)
  )

  result <- bulk_rename_folders(
    vol_id = TEST_VOL_ID,
    folder_ids = fids,
    new_names = new_names,
    vb = FALSE
  )

  skip_if_null_response(result, "bulk_rename_folders live API")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$input, fids)
  expect_true(all(result$status == "success"))

  for (k in seq_along(fids)) {
    fol <- get_folder_by_id(vol_id = TEST_VOL_ID, folder_id = fids[[k]], vb = FALSE)
    skip_if_null_response(fol, "get_folder_by_id after rename")
    expect_equal(fol$name, new_names[[k]])
  }
})

test_that("bulk_rename_folders fast-fails on bad folder id", {
  ids <- vapply(seq_len(2), function(i) {
    f <- create_folder(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_rename_folders resume %d", i),
      vb = FALSE
    )
    if (is.null(f)) NA_real_ else as.numeric(f$id)
  }, numeric(1))

  if (any(is.na(ids))) {
    testthat::skip("Could not create test folders on staging.")
  }
  on.exit({
    for (id in ids) {
      try(delete_folder(TEST_VOL_ID, id, vb = FALSE), silent = TRUE)
    }
  }, add = TRUE)

  mixed <- c(ids[[1]], TEST_MISSING_ID, ids[[2]])
  nn <- c("x", "y", "z")

  partial <- tryCatch(
    bulk_rename_folders(
      vol_id = TEST_VOL_ID,
      folder_ids = mixed,
      new_names = nn,
      vb = FALSE
    ),
    databraryr_bulk_error = function(e) e$partial
  )

  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[[1]], "success")
  expect_equal(partial$status[[2]], "failed")
  expect_equal(partial$status[[3]], "pending")
})

test_that("bulk_rename_folders on_error collect runs all rows", {
  ids <- vapply(seq_len(2), function(i) {
    f <- create_folder(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_rename_folders collect %d", i),
      vb = FALSE
    )
    if (is.null(f)) NA_real_ else as.numeric(f$id)
  }, numeric(1))

  if (any(is.na(ids))) {
    testthat::skip("Could not create test folders on staging.")
  }
  on.exit({
    for (id in ids) {
      try(delete_folder(TEST_VOL_ID, id, vb = FALSE), silent = TRUE)
    }
  }, add = TRUE)

  mixed <- c(ids[[1]], TEST_MISSING_ID, ids[[2]])
  nn <- c("c1", "c2", "c3")

  result <- bulk_rename_folders(
    vol_id = TEST_VOL_ID,
    folder_ids = mixed,
    new_names = nn,
    vb = FALSE,
    on_error = "collect"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, c("success", "failed", "success"))
})
