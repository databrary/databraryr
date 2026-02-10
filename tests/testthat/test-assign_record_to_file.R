# assign_record_to_file() ------------------------------------------------------
login_test_account()

test_that("assign_record_to_file assigns a record to a file", {
  # First create a record (unique name to avoid "already in use" across test runs)
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = sprintf("Assign test %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for assign test")

  record_id <- create_result$record_id

  # Get a session and file from volume 1
  sessions <- list_volume_sessions(vol_id = 1777, vb = FALSE)
  skip_if_null_response(sessions, "list_volume_sessions for assign test")

  if (nrow(sessions) > 0) {
    session_id <- sessions$session_id[1]

    # Get files from the session
    files <- list_session_assets(vol_id = 1777, session_id = session_id, vb = FALSE)
    
    if (!is.null(files) && nrow(files) > 0) {
      file_id <- files$asset_id[1]

      # Assign the record to the file
      assign_result <- assign_record_to_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )

      # Clean up - unassign first, then delete record
      unassign_record_from_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )
      delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

      skip_if_null_response(assign_result, "assign_record_to_file")

      expect_true(!is.null(assign_result))
    }
  }

  # Clean up record if test was skipped
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)
})

test_that("assign_record_to_file is idempotent", {
  # Create a record (unique name to avoid "already in use" across test runs)
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = sprintf("Assign idempotent %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for idempotent test")

  record_id <- create_result$record_id

  # Get a session and file
  sessions <- list_volume_sessions(vol_id = 1777, vb = FALSE)
  skip_if_null_response(sessions, "list_volume_sessions for idempotent test")

  if (nrow(sessions) > 0) {
    session_id <- sessions$session_id[1]
    files <- list_session_assets(vol_id = 1777, session_id = session_id, vb = FALSE)
    
    if (!is.null(files) && nrow(files) > 0) {
      file_id <- files$asset_id[1]

      # Assign twice
      assign_result1 <- assign_record_to_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )

      assign_result2 <- assign_record_to_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )

      # Clean up
      unassign_record_from_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )
      delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

      expect_true(!is.null(assign_result1))
      expect_true(!is.null(assign_result2))
    }
  }

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)
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
  expect_error(assign_record_to_file(vol_id = 1777, session_id = -1, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 0, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = "1", file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = TRUE, file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = c(1, 2), file_id = 1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1.5, file_id = 1, record_id = 1))
})

test_that("assign_record_to_file rejects invalid file_id", {
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = -1, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 0, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = "1", record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = TRUE, record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = c(1, 2), record_id = 1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1.5, record_id = 1))
})

test_that("assign_record_to_file rejects invalid record_id", {
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = -1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 0))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = "1"))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = TRUE))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = c(1, 2)))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1.5))
})

test_that("assign_record_to_file rejects invalid vb parameter", {
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = -1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = 3))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = "a"))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = NULL))
})

test_that("assign_record_to_file rejects invalid rq parameter", {
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, rq = "a"))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, rq = -1))
  expect_error(assign_record_to_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, rq = TRUE))
})
