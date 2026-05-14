# assign_record_to_file() ------------------------------------------------------
login_test_account()

test_that("assign_record_to_file assigns a record to a file", {
  sid <- make_test_session("assign_record_to_file session")
  skip_if_null_response(sid, "create_session for assign test")

  asset_name <- sprintf("assign_probe_%d.txt", sample(100000L:999999L, 1L))
  file_id <- upload_test_session_asset(sid, file_basename = asset_name)
  skip_if_null_response(file_id, "upload for assign test")

  create_result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = sprintf("Assign test %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for assign test")

  record_id <- create_result$record_id
  withr::defer(
    {
      try(
        unassign_record_from_file(
          vol_id = TEST_VOL_ID,
          session_id = sid,
          file_id = file_id,
          record_id = record_id,
          vb = FALSE
        ),
        silent = TRUE
      )
      try(
        delete_volume_record(vol_id = TEST_VOL_ID, record_id = record_id, vb = FALSE),
        silent = TRUE
      )
    },
    envir = parent.frame()
  )

  assign_result <- assign_record_to_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = FALSE
  )

  skip_if_null_response(assign_result, "assign_record_to_file")

  expect_true(!is.null(assign_result))
})

test_that("assign_record_to_file is idempotent", {
  sid <- make_test_session("assign_record_to_file idempotent session")
  skip_if_null_response(sid, "create_session for idempotent assign test")

  asset_name <- sprintf("assign_idem_%d.txt", sample(100000L:999999L, 1L))
  file_id <- upload_test_session_asset(sid, file_basename = asset_name)
  skip_if_null_response(file_id, "upload for idempotent assign test")

  create_result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = sprintf("Assign idempotent %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for idempotent test")

  record_id <- create_result$record_id
  withr::defer(
    {
      try(
        unassign_record_from_file(
          vol_id = TEST_VOL_ID,
          session_id = sid,
          file_id = file_id,
          record_id = record_id,
          vb = FALSE
        ),
        silent = TRUE
      )
      try(
        delete_volume_record(vol_id = TEST_VOL_ID, record_id = record_id, vb = FALSE),
        silent = TRUE
      )
    },
    envir = parent.frame()
  )

  assign_result1 <- assign_record_to_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = FALSE
  )

  assign_result2 <- assign_record_to_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = FALSE
  )

  expect_true(!is.null(assign_result1))
  expect_true(!is.null(assign_result2))
})

test_that("assign_record_to_file rejects invalid vol_id", {
  expect_error(assign_record_to_file(vol_id = -1, session_id = 1, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 0, session_id = 1, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = "1", session_id = 1, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TRUE, session_id = 1, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = c(1, 2), session_id = 1, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777.5, session_id = 1, file_id = 1, record_id = 1))
})

test_that("assign_record_to_file rejects invalid session_id", {
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = -1, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 0, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = "1", file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = TRUE, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = c(1, 2), file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1.5, file_id = 1, record_id = 1))
})

test_that("assign_record_to_file rejects invalid file_id", {
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = -1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 0, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = "1", record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = TRUE, record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = c(1, 2), record_id = 1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1.5, record_id = 1))
})

test_that("assign_record_to_file rejects invalid record_id", {
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = -1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 0))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = "1"))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = TRUE))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = c(1, 2)))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1.5))
})

test_that("assign_record_to_file rejects invalid vb parameter", {
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = -1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = 3))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = "a"))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = NULL))
})

test_that("assign_record_to_file rejects invalid rq parameter", {
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, rq = "a"))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, rq = -1))
  expect_error(assign_record_to_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, rq = TRUE))
})
