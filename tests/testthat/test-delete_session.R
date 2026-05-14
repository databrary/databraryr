# delete_session() -------------------------------------------------------------
login_test_account()

test_that("delete_session deletes an existing session", {
  created <- create_session(vol_id = TEST_VOL_ID, name = "delete_session happy path", vb = FALSE)
  skip_if_null_response(created, "create_session for delete_session happy path")

  session_id <- created$id
  on.exit(
    try(delete_session(vol_id = TEST_VOL_ID, session_id = session_id, vb = FALSE), silent = TRUE),
    add = TRUE
  )

  result <- delete_session(vol_id = TEST_VOL_ID, session_id = session_id, vb = FALSE)

  expect_true(result)

  expect_null(get_session_by_id(vol_id = TEST_VOL_ID, session_id = session_id, vb = FALSE))
})

test_that("delete_session returns FALSE for non-existent session", {
  expect_false(delete_session(vol_id = TEST_VOL_ID, session_id = 999999999, vb = FALSE))
})

test_that("delete_session works with verbose mode", {
  created <- create_session(vol_id = TEST_VOL_ID, name = "delete_session vb", vb = FALSE)
  skip_if_null_response(created, "create_session for delete_session vb")

  on.exit(
    try(delete_session(vol_id = TEST_VOL_ID, session_id = created$id, vb = FALSE), silent = TRUE),
    add = TRUE
  )

  expect_true(delete_session(vol_id = TEST_VOL_ID, session_id = created$id, vb = TRUE))
})

test_that("delete_session works with custom request object", {
  created <- create_session(
    vol_id = TEST_VOL_ID,
    name = "delete_session custom rq",
    vb = FALSE
  )
  skip_if_null_response(created, "create_session for delete_session custom rq")

  on.exit(
    try(delete_session(vol_id = TEST_VOL_ID, session_id = created$id, vb = FALSE), silent = TRUE),
    add = TRUE
  )

  custom_rq <- databraryr::make_default_request()
  expect_true(
    delete_session(
      vol_id = TEST_VOL_ID,
      session_id = created$id,
      rq = custom_rq,
      vb = FALSE
    )
  )
})

test_that("delete_session rejects invalid vol_id", {
  expect_error(delete_session(vol_id = -1, session_id = 1))
  expect_error(delete_session(vol_id = 0, session_id = 1))
  expect_error(delete_session(vol_id = "1", session_id = 1))
  expect_error(delete_session(vol_id = TRUE, session_id = 1))
  expect_error(delete_session(vol_id = list(a = 1), session_id = 1))
  expect_error(delete_session(vol_id = c(1, 2), session_id = 1))
  expect_error(delete_session(vol_id = 1.5, session_id = 1))
  expect_error(delete_session(vol_id = NULL, session_id = 1))
  expect_error(delete_session(vol_id = NA, session_id = 1))
})

test_that("delete_session rejects invalid session_id", {
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = -1))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 0))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = "1"))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = TRUE))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = list(a = 1)))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = c(1, 2)))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1.5))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = NULL))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = NA))
})

test_that("delete_session rejects invalid vb parameter", {
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, vb = -1))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, vb = 3))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, vb = "a"))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, vb = list(a = 1)))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, vb = c(TRUE, FALSE)))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, vb = NULL))
})

test_that("delete_session rejects invalid rq parameter", {
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, rq = "a"))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, rq = -1))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, rq = c(2, 3)))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, rq = list(a = 1)))
  expect_error(delete_session(vol_id = TEST_VOL_ID, session_id = 1, rq = TRUE))
})
