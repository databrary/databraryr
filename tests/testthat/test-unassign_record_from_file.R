# unassign_record_from_file() --------------------------------------------------
login_test_account()

test_that("unassign_record_from_file unassigns a record from a file", {
  sid <- make_test_session("unassign_record happy session")
  skip_if_null_response(sid, "create_session for unassign test")

  asset_name <- sprintf("unassign_probe_%d.txt", sample(100000L:999999L, 1L))
  file_id <- upload_test_session_asset(sid, file_basename = asset_name)
  skip_if_null_response(file_id, "upload for unassign test")

  create_result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = sprintf("Unassign test %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for unassign test")

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

  assign_record_to_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = FALSE
  )

  unassign_result <- unassign_record_from_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = FALSE
  )

  expect_true(unassign_result)
})

test_that("unassign_record_from_file returns FALSE for non-assigned record", {
  sid <- make_test_session("unassign_record fail session")
  skip_if_null_response(sid, "create_session for unassign fail test")

  asset_name <- sprintf("unassign_fail_%d.txt", sample(100000L:999999L, 1L))
  file_id <- upload_test_session_asset(sid, file_basename = asset_name)
  skip_if_null_response(file_id, "upload for unassign fail test")

  create_result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = sprintf("Unassign fail %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for unassign fail test")

  record_id <- create_result$record_id
  withr::defer(
    try(delete_volume_record(vol_id = TEST_VOL_ID, record_id = record_id, vb = FALSE), silent = TRUE),
    envir = parent.frame()
  )

  unassign_result <- unassign_record_from_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = FALSE
  )

  expect_false(unassign_result)
})

test_that("unassign_record_from_file works with verbose mode", {
  sid <- make_test_session("unassign_record verbose session")
  skip_if_null_response(sid, "create_session for unassign verbose test")

  asset_name <- sprintf("unassign_verbose_%d.txt", sample(100000L:999999L, 1L))
  file_id <- upload_test_session_asset(sid, file_basename = asset_name)
  skip_if_null_response(file_id, "upload for unassign verbose test")

  create_result <- create_volume_record(
    vol_id = TEST_VOL_ID,
    category_id = TEST_CATEGORY_ID,
    name = sprintf("Unassign verbose %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for unassign verbose test")

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

  assign_record_to_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = FALSE
  )

  unassign_result <- unassign_record_from_file(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    file_id = file_id,
    record_id = record_id,
    vb = TRUE
  )

  expect_true(unassign_result)
})

test_that("unassign_record_from_file rejects invalid vol_id", {
  expect_error(unassign_record_from_file(vol_id = -1, session_id = 1, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 0, session_id = 1, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = "1", session_id = 1, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TRUE, session_id = 1, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = c(1, 2), session_id = 1, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777.5, session_id = 1, file_id = 1, record_id = 1))
})

test_that("unassign_record_from_file rejects invalid session_id", {
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = -1, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 0, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = "1", file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = TRUE, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = c(1, 2), file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1.5, file_id = 1, record_id = 1))
})

test_that("unassign_record_from_file rejects invalid file_id", {
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = -1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 0, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = "1", record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = TRUE, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = c(1, 2), record_id = 1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1.5, record_id = 1))
})

test_that("unassign_record_from_file rejects invalid record_id", {
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = -1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 0))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = "1"))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = TRUE))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = c(1, 2)))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1.5))
})

test_that("unassign_record_from_file rejects invalid vb parameter", {
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = -1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = 3))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = "a"))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, vb = NULL))
})

test_that("unassign_record_from_file rejects invalid rq parameter", {
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, rq = "a"))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, rq = -1))
  expect_error(unassign_record_from_file(vol_id = TEST_VOL_ID, session_id = 1, file_id = 1, record_id = 1, rq = TRUE))
})
