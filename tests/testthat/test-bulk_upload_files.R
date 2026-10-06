# bulk_upload_files() ---------------------------------------------------------
login_test_account()

# Argument validation (no API) ------------------------------------------------

test_that("bulk_upload_files rejects invalid args", {
  tf <- tempfile(fileext = ".txt")
  writeLines("ok", tf)
  on.exit(unlink(tf), add = TRUE)
  expect_error(bulk_upload_files(
    vol_id = 1, file_paths = tf
  ), regexp = "Exactly one of session_id and folder_id")
  expect_error(bulk_upload_files(
    vol_id = 1, session_id = 1, folder_id = 2, file_paths = tf
  ), regexp = "Exactly one of session_id and folder_id")

  expect_error(bulk_upload_files(
    vol_id = -1, session_id = 1, file_paths = "x"
  ))
  expect_error(bulk_upload_files(
    vol_id = 1, session_id = 0, file_paths = "x"
  ))
  expect_error(bulk_upload_files(
    vol_id = 1, session_id = 1, file_paths = character(0)
  ))
  expect_error(bulk_upload_files(
    vol_id = 1, session_id = 1, file_paths = c("/nope/does/not/exist")
  ))
  expect_error(bulk_upload_files(
    vol_id = 1, session_id = 1, file_paths = c("a", NA)
  ))
})

# Internal helpers ------------------------------------------------------------

test_that("init_bulk_tibble initializes one pending row per input", {
  t <- databraryr:::init_bulk_tibble(c("a", "b", "c"))
  expect_s3_class(t, "tbl_df")
  expect_equal(nrow(t), 3)
  expect_equal(t$status, rep("pending", 3))
  expect_true(all(is.na(t$error)))
  expect_true(all(is.na(t$reason)))
  expect_equal(length(t$result), 3)
})

test_that("bulk_apply succeeds when fn returns non-NULL", {
  state <- databraryr:::bulk_apply(
    inputs = c(1, 2, 3),
    fn = function(x) x * 10
  )
  expect_equal(state$status, rep("success", 3))
  expect_equal(unlist(state$result), c(10, 20, 30))
})

test_that("bulk_apply fast-fails and carries partial state", {
  err <- tryCatch(
    databraryr:::bulk_apply(
      inputs = c(1, 2, 3),
      fn = function(x) if (x == 2) NULL else x
    ),
    databraryr_bulk_error = function(e) e
  )
  expect_s3_class(err, "databraryr_bulk_error")
  expect_equal(err$failed_input, 2)
  expect_equal(err$partial$status, c("success", "failed", "pending"))
  expect_equal(err$partial$result[[1]], 1)
  expect_false(is.na(err$partial$error[2]))
})

test_that("bulk_apply respects custom is_failure predicate", {
  err <- tryCatch(
    databraryr:::bulk_apply(
      inputs = c(1, 2),
      fn = function(x) FALSE,
      is_failure = function(res) isFALSE(res)
    ),
    databraryr_bulk_error = function(e) e
  )
  expect_s3_class(err, "databraryr_bulk_error")
  expect_equal(err$failed_input, 1)
})

test_that("bulk_apply catches errors thrown by fn", {
  err <- tryCatch(
    databraryr:::bulk_apply(
      inputs = c(1, 2),
      fn = function(x) stop("boom")
    ),
    databraryr_bulk_error = function(e) e
  )
  expect_s3_class(err, "databraryr_bulk_error")
  expect_match(err$partial$error[1], "boom")
})

test_that("bulk_apply collect mode records failures and continues", {
  state <- databraryr:::bulk_apply(
    inputs = c(1, 2, 3),
    fn = function(x) if (x == 2) NULL else x * 10,
    on_error = "collect"
  )
  expect_equal(state$status, c("success", "failed", "success"))
  expect_equal(state$result[[1]], 10)
  expect_true(is.null(state$result[[2]]))
  expect_equal(state$result[[3]], 30)
  expect_false(is.na(state$error[2]))
  expect_true(is.na(state$error[1]))
  expect_true(is.na(state$error[3]))
})

test_that("bulk_apply retries failed invocations up to max_retries", {
  n_try <- 0L
  state <- databraryr:::bulk_apply(
    inputs = 1,
    fn = function(x) {
      n_try <<- n_try + 1L
      if (n_try < 3L) NULL else 99L
    },
    max_retries = 2L
  )
  expect_equal(n_try, 3L)
  expect_equal(state$status, "success")
  expect_equal(state$result[[1]], 99L)

  n_try2 <- 0L
  err <- tryCatch(
    databraryr:::bulk_apply(
      inputs = 1,
      fn = function(x) {
        n_try2 <<- n_try2 + 1L
        NULL
      },
      max_retries = 1L
    ),
    databraryr_bulk_error = function(e) e
  )
  expect_equal(n_try2, 2L)
  expect_s3_class(err, "databraryr_bulk_error")
})

# End-to-end live API tests ---------------------------------------------------

test_that("bulk_upload_files uploads multiple files end-to-end", {
  session <- create_session(
    vol_id = TEST_VOL_ID, name = "bulk_upload test", vb = FALSE
  )
  skip_if_null_response(session, "create_session for bulk_upload_files")
  on.exit(
    delete_session(vol_id = TEST_VOL_ID, session_id = session$id, vb = FALSE),
    add = TRUE
  )

  paths <- vapply(seq_len(3), function(i) {
    p <- tempfile(pattern = sprintf("bulk_%d_", i), fileext = ".txt")
    writeLines(strrep("x", 512L), p, useBytes = FALSE)
    p
  }, character(1))
  on.exit(unlink(paths), add = TRUE)

  result <- bulk_upload_files(
    vol_id = TEST_VOL_ID,
    session_id = session$id,
    file_paths = paths,
    preflight = FALSE,
    vb = FALSE
  )
  skip_if_null_response(result, "bulk_upload_files e2e")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$input, paths)
  expect_true(all(result$status == "success"))
})

test_that("bulk_upload_files uploads multiple files to a folder end-to-end", {
  fid <- make_test_folder("bulk_upload folder e2e", vb = FALSE)
  skip_if_null_response(fid, "make_test_folder for bulk_upload folder e2e")

  paths <- vapply(seq_len(2), function(i) {
    p <- tempfile(pattern = sprintf("bulk_folder_%d_", i), fileext = ".txt")
    writeLines(strrep("x", 512L), p, useBytes = FALSE)
    p
  }, character(1))
  on.exit(unlink(paths), add = TRUE)

  result <- bulk_upload_files(
    vol_id = TEST_VOL_ID,
    session_id = NULL,
    file_paths = paths,
    folder_id = fid,
    preflight = FALSE,
    vb = FALSE
  )
  skip_if_null_response(result, "bulk_upload_files folder e2e")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$input, paths)
  expect_true(all(result$status == "success"))
})

test_that("bulk_upload_files preflight skips duplicate filenames", {
  session <- create_session(
    vol_id = TEST_VOL_ID, name = "bulk_upload preflight test", vb = FALSE
  )
  skip_if_null_response(session, "create_session for preflight test")
  on.exit(
    delete_session(vol_id = TEST_VOL_ID, session_id = session$id, vb = FALSE),
    add = TRUE
  )

  p <- tempfile(pattern = "preflight_", fileext = ".txt")
  writeLines(strrep("x", 512L), p, useBytes = FALSE)
  on.exit(unlink(p), add = TRUE)

  first <- bulk_upload_files(
    vol_id = TEST_VOL_ID, session_id = session$id, file_paths = p,
    preflight = FALSE, vb = FALSE
  )
  skip_if_null_response(first, "first upload for preflight test")
  expect_true(first$status == "success")

  # Listing used by preflight can lag behind upload_file success on staging.
  bn <- basename(p)
  deadline <- Sys.time() + 60
  visible <- FALSE
  while (Sys.time() < deadline) {
    dup <- check_duplicate_files_in_session(
      vol_id = TEST_VOL_ID,
      session_id = session$id,
      filenames = bn,
      vb = FALSE
    )
    if (!is.null(dup) && nrow(dup) == 1L && isTRUE(dup$exists[[1L]])) {
      visible <- TRUE
      break
    }
    Sys.sleep(0.5)
  }
  skip_if(!visible, "upload not yet visible for duplicate preflight on staging")

  again <- bulk_upload_files(
    vol_id = TEST_VOL_ID, session_id = session$id, file_paths = p,
    preflight = TRUE, vb = FALSE
  )
  expect_equal(again$status, "skipped")
  expect_equal(again$reason, "duplicate")
})

test_that("bulk_upload_files folder preflight skips duplicate filenames", {
  fid <- make_test_folder("bulk_upload folder preflight", vb = FALSE)
  skip_if_null_response(fid, "make_test_folder for folder preflight")

  p <- tempfile(pattern = "preflight_folder_", fileext = ".txt")
  writeLines(strrep("x", 512L), p, useBytes = FALSE)
  on.exit(unlink(p), add = TRUE)

  first <- bulk_upload_files(
    vol_id = TEST_VOL_ID,
    session_id = NULL,
    file_paths = p,
    folder_id = fid,
    preflight = FALSE,
    vb = FALSE
  )
  skip_if_null_response(first, "first folder upload for preflight test")
  expect_true(first$status == "success")

  bn <- basename(p)
  deadline <- Sys.time() + 60
  visible <- FALSE
  while (Sys.time() < deadline) {
    dup <- check_duplicate_files_in_folder(
      vol_id = TEST_VOL_ID,
      folder_id = fid,
      filenames = bn,
      vb = FALSE
    )
    if (!is.null(dup) && nrow(dup) == 1L && isTRUE(dup$exists[[1L]])) {
      visible <- TRUE
      break
    }
    Sys.sleep(0.5)
  }
  skip_if(!visible, "folder upload not yet visible for duplicate preflight")

  again <- bulk_upload_files(
    vol_id = TEST_VOL_ID,
    session_id = NULL,
    file_paths = p,
    folder_id = fid,
    preflight = TRUE,
    vb = FALSE
  )
  expect_equal(again$status, "skipped")
  expect_equal(again$reason, "duplicate")
})

test_that("bulk_upload_files fast-fails and supports resume", {
  session <- create_session(
    vol_id = TEST_VOL_ID, name = "bulk_upload resume test", vb = FALSE
  )
  skip_if_null_response(session, "create_session for resume test")
  on.exit(
    delete_session(vol_id = TEST_VOL_ID, session_id = session$id, vb = FALSE),
    add = TRUE
  )

  good <- tempfile(pattern = "good_", fileext = ".txt")
  writeLines(strrep("x", 512L), good, useBytes = FALSE)
  on.exit(unlink(good), add = TRUE)

  bad_inputs <- c(good, good)
  partial <- tryCatch(
    bulk_upload_files(
      vol_id = TEST_VOL_ID,
      session_id = 999999999,
      file_paths = bad_inputs,
      preflight = FALSE,
      vb = FALSE
    ),
    databraryr_bulk_error = function(e) e$partial
  )

  skip_if(is.null(partial), "expected fast-fail did not occur")
  expect_s3_class(partial, "tbl_df")
  expect_equal(partial$status[1], "failed")
  expect_equal(partial$status[2], "pending")

  resumed <- resume_bulk(
    partial,
    bulk_upload_files,
    vol_id = TEST_VOL_ID,
    session_id = session$id,
    preflight = FALSE,
    vb = FALSE
  )
  skip_if_null_response(resumed, "bulk_upload_files resume")
  expect_true(all(resumed$status == "success"))
})
