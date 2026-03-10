# unassign_record_from_file() --------------------------------------------------
login_test_account()

test_that("unassign_record_from_file unassigns a record from a file", {
  # Create a record (unique name to avoid "already in use" across test runs)
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = sprintf("Unassign test %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for unassign test")

  record_id <- create_result$record_id

  # Get a session and file
  sessions <- list_volume_sessions(vol_id = 1777, vb = FALSE)
  skip_if_null_response(sessions, "list_volume_sessions for unassign test")

  if (nrow(sessions) > 0) {
    session_id <- sessions$session_id[1]
    files <- list_session_assets(vol_id = 1777, session_id = session_id, vb = FALSE)
    
    if (!is.null(files) && nrow(files) > 0) {
      file_id <- files$asset_id[1]

      # Assign first
      assign_record_to_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )

      # Then unassign
      unassign_result <- unassign_record_from_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )

      # Clean up
      delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

      expect_true(unassign_result)
    }
  }

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)
})

test_that("unassign_record_from_file returns FALSE for non-assigned record", {
  # Create a record (unique name to avoid "already in use" across test runs)
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = sprintf("Unassign fail %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for unassign fail test")

  record_id <- create_result$record_id

  # Get a session and file
  sessions <- list_volume_sessions(vol_id = 1777, vb = FALSE)
  skip_if_null_response(sessions, "list_volume_sessions for unassign fail test")

  if (nrow(sessions) > 0) {
    session_id <- sessions$session_id[1]
    files <- list_session_assets(vol_id = 1777, session_id = session_id, vb = FALSE)
    
    if (!is.null(files) && nrow(files) > 0) {
      file_id <- files$asset_id[1]

      # Try to unassign without assigning first
      unassign_result <- unassign_record_from_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )

      # Clean up
      delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

      expect_false(unassign_result)
    }
  }

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)
})

test_that("unassign_record_from_file works with verbose mode", {
  # Create a record (unique name to avoid "already in use" across test runs)
  create_result <- create_volume_record(
    vol_id = 1777,
    category_id = 6,
    name = sprintf("Unassign verbose %d", sample(100000L:999999L, 1L)),
    vb = FALSE
  )
  skip_if_null_response(create_result, "create_volume_record for unassign verbose test")

  record_id <- create_result$record_id

  # Get a session and file
  sessions <- list_volume_sessions(vol_id = 1777, vb = FALSE)
  skip_if_null_response(sessions, "list_volume_sessions for unassign verbose test")

  if (nrow(sessions) > 0) {
    session_id <- sessions$session_id[1]
    files <- list_session_assets(vol_id = 1777, session_id = session_id, vb = FALSE)
    
    if (!is.null(files) && nrow(files) > 0) {
      file_id <- files$asset_id[1]

      # Assign first
      assign_record_to_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = FALSE
      )

      # Unassign with verbose
      unassign_result <- unassign_record_from_file(
        vol_id = 1777,
        session_id = session_id,
        file_id = file_id,
        record_id = record_id,
        vb = TRUE
      )

      # Clean up
      delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)

      expect_true(unassign_result)
    }
  }

  # Clean up
  delete_volume_record(vol_id = 1777, record_id = record_id, vb = FALSE)
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
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = -1, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 0, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = "1", file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = TRUE, file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = c(1, 2), file_id = 1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1.5, file_id = 1, record_id = 1))
})

test_that("unassign_record_from_file rejects invalid file_id", {
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = -1, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 0, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = "1", record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = TRUE, record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = c(1, 2), record_id = 1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1.5, record_id = 1))
})

test_that("unassign_record_from_file rejects invalid record_id", {
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = -1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 0))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = "1"))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = TRUE))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = c(1, 2)))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1.5))
})

test_that("unassign_record_from_file rejects invalid vb parameter", {
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = -1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = 3))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = "a"))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, vb = NULL))
})

test_that("unassign_record_from_file rejects invalid rq parameter", {
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, rq = "a"))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, rq = -1))
  expect_error(unassign_record_from_file(vol_id = 1777, session_id = 1, file_id = 1, record_id = 1, rq = TRUE))
})
