# bulk_rename_files() ---------------------------------------------------------
login_test_account()

test_that("bulk_rename_files rejects invalid args", {
  expect_error(bulk_rename_files(vol_id = -1, session_id = 1L, file_ids = 1L, new_names = "a"))
  expect_error(bulk_rename_files(vol_id = 1L, session_id = 1L, file_ids = integer(0), new_names = character(0)))
  expect_error(bulk_rename_files(vol_id = 1L, session_id = 1L, file_ids = c(1L, 2L), new_names = "a"))
  expect_error(bulk_rename_files(vol_id = 1L, session_id = 1L, file_ids = c(1L, 1L), new_names = c("a", "b")))
})

test_that("bulk_rename_files renames multiple session assets", {
  uniq <- sprintf(
    "bulk_rfile_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  sid <- make_test_session(sprintf("%s_sess", uniq), vb = FALSE)
  skip_if_null_response(sid, "make_test_session for bulk_rename_files")

  fn1 <- sprintf("%s_one.txt", uniq)
  fn2 <- sprintf("%s_two.txt", uniq)
  aid1 <- upload_test_session_asset(sid, file_basename = fn1, vb = FALSE)
  aid2 <- upload_test_session_asset(sid, file_basename = fn2, vb = FALSE)
  skip_if_null_response(aid1, "upload asset 1")
  skip_if_null_response(aid2, "upload asset 2")

  new_names <- c(
    sprintf("%s_one_renamed.txt", uniq),
    sprintf("%s_two_renamed.txt", uniq)
  )

  result <- bulk_rename_files(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_ids = c(aid1, aid2),
    new_names = new_names,
    vb = FALSE
  )

  skip_if_null_response(result, "bulk_rename_files live API")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$input, c(aid1, aid2))
  expect_true(all(result$status == "success"))

  file_ids <- c(aid1, aid2)
  for (k in seq_along(file_ids)) {
    file_detail <- get_session_file(
      vol_id = TEST_VOL_ID,
      session_id = sid,
      file_id = file_ids[[k]],
      vb = FALSE
    )
    skip_if_null_response(
      file_detail,
      sprintf("get_session_file after rename (file %d)", k)
    )
    nm <- new_names[[k]]
    expect_true(
      identical(file_detail$name, nm) ||
        identical(file_detail$name, tools::file_path_sans_ext(nm)),
      info = sprintf(
        'Expected name "%s" or stem "%s"; got "%s"',
        nm, tools::file_path_sans_ext(nm), file_detail$name
      )
    )
  }
})

test_that("bulk_rename_files fast-fails on bad file id", {
  sid <- make_test_session("bulk_rename_files resume sess", vb = FALSE)
  skip_if_null_response(sid, "make_test_session")

  fn1 <- "bulk_rf_resume_a.txt"
  aid <- upload_test_session_asset(sid, file_basename = fn1, vb = FALSE)
  skip_if_null_response(aid, "upload asset")

  mixed <- c(aid, TEST_MISSING_ID)
  nn <- c("bulk_rf_resume_a_renamed.txt", "bulk_rf_resume_b.txt")

  partial <- tryCatch(
    bulk_rename_files(
      vol_id = TEST_VOL_ID,
      session_id = sid,
      file_ids = mixed,
      new_names = nn,
      vb = FALSE
    ),
    databraryr_bulk_error = function(e) e$partial
  )

  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[[1]], "success")
  expect_equal(partial$status[[2]], "failed")
})

test_that("bulk_rename_files on_error collect runs all rows", {
  sid <- make_test_session("bulk_rename_files collect sess", vb = FALSE)
  skip_if_null_response(sid, "make_test_session")

  fn1 <- "bulk_rf_collect_a.txt"
  fn2 <- "bulk_rf_collect_b.txt"
  aid1 <- upload_test_session_asset(sid, file_basename = fn1, vb = FALSE)
  aid2 <- upload_test_session_asset(sid, file_basename = fn2, vb = FALSE)
  skip_if_null_response(aid1, "upload 1")
  skip_if_null_response(aid2, "upload 2")

  mixed <- c(aid1, TEST_MISSING_ID, aid2)
  nn <- c("bulk_rf_c1.txt", "bulk_rf_c2.txt", "bulk_rf_c3.txt")

  result <- bulk_rename_files(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_ids = mixed,
    new_names = nn,
    vb = FALSE,
    on_error = "collect"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, c("success", "failed", "success"))
})
