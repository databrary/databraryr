# add_default_record_to_session() ----------------------------------------------
login_test_account()

# Sandbox volume 1777, category 6 ("task") is used elsewhere for record tests.
TEST_VOL <- 1777
TEST_CATEGORY <- 6

new_session_id <- function(name = "add_default_record test") {
  created <- create_session(vol_id = TEST_VOL, name = name, vb = FALSE)
  if (is.null(created)) {
    return(NULL)
  }
  created$id
}

new_record_id <- function(name = "add_default_record test record") {
  created <- create_volume_record(
    vol_id = TEST_VOL,
    category_id = TEST_CATEGORY,
    name = name,
    vb = FALSE
  )
  if (is.null(created)) {
    return(NULL)
  }
  created$record_id
}

test_that("add_default_record_to_session attaches a record", {
  sid <- new_session_id("add_default_record happy path")
  skip_if_null_response(sid, "create_session for add_default_record happy path")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  rid <- new_record_id("add_default_record happy path record")
  skip_if_null_response(rid, "create_volume_record for add_default_record happy path")
  on.exit(
    delete_volume_record(vol_id = TEST_VOL, record_id = rid, vb = FALSE),
    add = TRUE
  )

  result <- add_default_record_to_session(
    vol_id = TEST_VOL,
    session_id = sid,
    record_id = rid,
    vb = FALSE
  )
  expect_true(result)

  # Verify it shows up among the session's default_records
  session <- get_session_by_id(
    vol_id = TEST_VOL,
    session_id = sid,
    vb = FALSE
  )
  default_ids <- vapply(
    session$default_records,
    function(r) as.integer(r$id),
    integer(1)
  )
  expect_true(as.integer(rid) %in% default_ids)
})

test_that("add_default_record_to_session returns FALSE for non-existent record", {
  sid <- new_session_id("add_default_record non-existent record")
  skip_if_null_response(sid, "create_session for non-existent record test")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  expect_false(
    add_default_record_to_session(
      vol_id = TEST_VOL,
      session_id = sid,
      record_id = 999999999,
      vb = FALSE
    )
  )
})

test_that("add_default_record_to_session returns FALSE for non-existent session", {
  rid <- new_record_id("add_default_record non-existent session record")
  skip_if_null_response(rid, "create_volume_record for non-existent session test")
  on.exit(
    delete_volume_record(vol_id = TEST_VOL, record_id = rid, vb = FALSE),
    add = TRUE
  )

  expect_false(
    add_default_record_to_session(
      vol_id = TEST_VOL,
      session_id = 999999999,
      record_id = rid,
      vb = FALSE
    )
  )
})

test_that("add_default_record_to_session works with verbose mode", {
  sid <- new_session_id("add_default_record vb")
  skip_if_null_response(sid, "create_session for add_default_record vb")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  rid <- new_record_id("add_default_record vb record")
  skip_if_null_response(rid, "create_volume_record for add_default_record vb")
  on.exit(
    delete_volume_record(vol_id = TEST_VOL, record_id = rid, vb = FALSE),
    add = TRUE
  )

  expect_true(
    add_default_record_to_session(
      vol_id = TEST_VOL,
      session_id = sid,
      record_id = rid,
      vb = TRUE
    )
  )
})

test_that("add_default_record_to_session works with custom request object", {
  sid <- new_session_id("add_default_record custom rq")
  skip_if_null_response(sid, "create_session for add_default_record custom rq")
  on.exit(delete_session(vol_id = TEST_VOL, session_id = sid, vb = FALSE), add = TRUE)

  rid <- new_record_id("add_default_record custom rq record")
  skip_if_null_response(rid, "create_volume_record for add_default_record custom rq")
  on.exit(
    delete_volume_record(vol_id = TEST_VOL, record_id = rid, vb = FALSE),
    add = TRUE
  )

  custom_rq <- databraryr::make_default_request()
  expect_true(
    add_default_record_to_session(
      vol_id = TEST_VOL,
      session_id = sid,
      record_id = rid,
      rq = custom_rq,
      vb = FALSE
    )
  )
})

test_that("add_default_record_to_session rejects invalid vol_id", {
  expect_error(add_default_record_to_session(vol_id = -1, session_id = 1, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = 0, session_id = 1, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = "1", session_id = 1, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = TRUE, session_id = 1, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = c(1, 2), session_id = 1, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = 1.5, session_id = 1, record_id = 1))
})

test_that("add_default_record_to_session rejects invalid session_id", {
  expect_error(add_default_record_to_session(vol_id = 1, session_id = -1, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 0, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = "1", record_id = 1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = TRUE, record_id = 1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = c(1, 2), record_id = 1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1.5, record_id = 1))
})

test_that("add_default_record_to_session rejects invalid record_id", {
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = -1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 0))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = "1"))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = TRUE))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = c(1, 2)))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1.5))
})

test_that("add_default_record_to_session rejects invalid vb parameter", {
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1, vb = -1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1, vb = "a"))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1, vb = c(TRUE, FALSE)))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1, vb = NULL))
})

test_that("add_default_record_to_session rejects invalid rq parameter", {
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1, rq = "a"))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1, rq = -1))
  expect_error(add_default_record_to_session(vol_id = 1, session_id = 1, record_id = 1, rq = TRUE))
})
