# bulk_rename_sessions() ------------------------------------------------------
login_test_account()

test_that("bulk_rename_sessions rejects invalid args", {
  expect_error(bulk_rename_sessions(vol_id = -1, session_ids = 1L, new_names = "a"))
  expect_error(bulk_rename_sessions(vol_id = 1, session_ids = integer(0), new_names = character(0)))
  expect_error(bulk_rename_sessions(vol_id = 1L, session_ids = c(1L, 2L), new_names = "a"))
  expect_error(bulk_rename_sessions(vol_id = 1L, session_ids = c(1L, 1L), new_names = c("a", "b")))
})

test_that("bulk_rename_sessions renames multiple sessions", {
  uniq <- sprintf(
    "bulk_rs_%s_%s",
    as.integer(Sys.time()),
    paste(sample(letters, 6, replace = TRUE), collapse = "")
  )
  sids <- integer(3)
  for (i in seq_along(sids)) {
    nm <- sprintf("%s_%d", uniq, i)
    sid <- make_test_session(nm, vb = FALSE)
    skip_if_null_response(sid, "make_test_session for bulk_rename_sessions")
    sids[[i]] <- sid
  }

  new_names <- c(
    sprintf("%s_new_a", uniq),
    sprintf("%s_new_b", uniq),
    sprintf("%s_new_c", uniq)
  )

  result <- bulk_rename_sessions(
    vol_id = TEST_VOL_ID,
    session_ids = sids,
    new_names = new_names,
    vb = FALSE
  )

  skip_if_null_response(result, "bulk_rename_sessions live API")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(result$input, sids)
  expect_true(all(result$status == "success"))

  for (k in seq_along(sids)) {
    sess <- get_session_by_id(vol_id = TEST_VOL_ID, session_id = sids[[k]], vb = FALSE)
    skip_if_null_response(sess, "get_session_by_id after rename")
    expect_equal(sess$name, new_names[[k]])
  }
})

test_that("bulk_rename_sessions fast-fails on bad session id", {
  ids <- vapply(seq_len(2), function(i) {
    s <- create_session(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_rename_sessions resume %d", i),
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

  mixed <- c(ids[[1]], TEST_MISSING_ID, ids[[2]])
  nn <- c("x", "y", "z")

  partial <- tryCatch(
    bulk_rename_sessions(
      vol_id = TEST_VOL_ID,
      session_ids = mixed,
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

test_that("bulk_rename_sessions on_error collect runs all rows", {
  ids <- vapply(seq_len(2), function(i) {
    s <- create_session(
      vol_id = TEST_VOL_ID,
      name = sprintf("bulk_rename_sessions collect %d", i),
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

  mixed <- c(ids[[1]], TEST_MISSING_ID, ids[[2]])
  nn <- c("c1", "c2", "c3")

  result <- bulk_rename_sessions(
    vol_id = TEST_VOL_ID,
    session_ids = mixed,
    new_names = nn,
    vb = FALSE,
    on_error = "collect"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$status, c("success", "failed", "success"))
})
