# remove_default_record_from_session() -----------------------------------------
login_test_account()

# create_volume_record name metric must be unique per volume; use a random
# suffix so repeated runs / leaked rows do not return HTTP 400 and NULL.
setup_attached <- function(name) {
  envir <- parent.frame()
  sfx <- sample(100000L:999999L, 1L)
  sid <- make_test_session(sprintf("%s session %d", name, sfx), envir = envir)
  if (is.null(sid)) {
    return(NULL)
  }

  rid <- make_test_record(sprintf("%s record %d", name, sfx), envir = envir)
  if (is.null(rid)) {
    return(NULL)
  }

  attached <- add_default_record_to_session(
    vol_id = TEST_VOL_ID,
    session_id = sid,
    record_id = rid,
    vb = FALSE
  )

  if (!isTRUE(attached)) {
    return(NULL)
  }

  list(session_id = sid, record_id = rid)
}

test_that("remove_default_record_from_session detaches a record", {
  setup <- setup_attached("remove_default_record happy path")
  skip_if_null_response(setup, "setup for remove_default_record happy path")

  result <- remove_default_record_from_session(
    vol_id = TEST_VOL_ID,
    session_id = setup$session_id,
    record_id = setup$record_id,
    vb = FALSE
  )
  expect_true(result)

  # Verify the record is no longer among default_records
  session <- get_session_by_id(
    vol_id = TEST_VOL_ID,
    session_id = setup$session_id,
    vb = FALSE
  )
  default_ids <- vapply(
    session$default_records,
    function(r) as.integer(r$id),
    integer(1)
  )
  expect_false(as.integer(setup$record_id) %in% default_ids)
})

test_that("remove_default_record_from_session returns FALSE for unattached record", {
  sfx <- sample(100000L:999999L, 1L)
  sid <- make_test_session(sprintf("remove_default_record unattached %d", sfx))
  skip_if_null_response(sid, "create_session for unattached test")

  rid <- make_test_record(sprintf("remove_default_record unattached record %d", sfx))
  skip_if_null_response(rid, "create_volume_record for unattached test")

  expect_false(
    remove_default_record_from_session(
      vol_id = TEST_VOL_ID,
      session_id = sid,
      record_id = rid,
      vb = FALSE
    )
  )
})

test_that("remove_default_record_from_session returns FALSE for non-existent record", {
  sfx <- sample(100000L:999999L, 1L)
  sid <- make_test_session(sprintf("remove_default_record non-existent record %d", sfx))
  skip_if_null_response(sid, "create_session for non-existent record test")

  expect_false(
    remove_default_record_from_session(
      vol_id = TEST_VOL_ID,
      session_id = sid,
      record_id = 999999999,
      vb = FALSE
    )
  )
})

test_that("remove_default_record_from_session returns FALSE for non-existent session", {
  sfx <- sample(100000L:999999L, 1L)
  rid <- make_test_record(sprintf("remove_default_record non-existent session record %d", sfx))
  skip_if_null_response(rid, "create_volume_record for non-existent session test")

  expect_false(
    remove_default_record_from_session(
      vol_id = TEST_VOL_ID,
      session_id = 999999999,
      record_id = rid,
      vb = FALSE
    )
  )
})

test_that("remove_default_record_from_session works with verbose mode", {
  setup <- setup_attached("remove_default_record vb")
  skip_if_null_response(setup, "setup for remove_default_record vb")

  expect_true(
    remove_default_record_from_session(
      vol_id = TEST_VOL_ID,
      session_id = setup$session_id,
      record_id = setup$record_id,
      vb = TRUE
    )
  )
})

test_that("remove_default_record_from_session works with custom request object", {
  setup <- setup_attached("remove_default_record custom rq")
  skip_if_null_response(setup, "setup for remove_default_record custom rq")

  custom_rq <- databraryr::make_default_request()
  expect_true(
    remove_default_record_from_session(
      vol_id = TEST_VOL_ID,
      session_id = setup$session_id,
      record_id = setup$record_id,
      rq = custom_rq,
      vb = FALSE
    )
  )
})

test_that("remove_default_record_from_session rejects invalid vol_id", {
  expect_error(remove_default_record_from_session(vol_id = -1, session_id = 1, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = 0, session_id = 1, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = "1", session_id = 1, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = TRUE, session_id = 1, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = c(1, 2), session_id = 1, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = 1.5, session_id = 1, record_id = 1))
})

test_that("remove_default_record_from_session rejects invalid session_id", {
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = -1, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 0, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = "1", record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = TRUE, record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = c(1, 2), record_id = 1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1.5, record_id = 1))
})

test_that("remove_default_record_from_session rejects invalid record_id", {
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = -1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 0))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = "1"))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = TRUE))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = c(1, 2)))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1.5))
})

test_that("remove_default_record_from_session rejects invalid vb parameter", {
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1, vb = -1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1, vb = "a"))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1, vb = NULL))
})

test_that("remove_default_record_from_session rejects invalid rq parameter", {
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1, rq = "a"))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1, rq = -1))
  expect_error(remove_default_record_from_session(vol_id = 1, session_id = 1, record_id = 1, rq = TRUE))
})
