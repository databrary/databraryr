# delete_session() -------------------------------------------------------------
login_test_account()

# Internal helper: create a session directly via the API for roundtrip tests.
# Not using create_session() yet to keep this block self-contained; switch
# once create_session() lands.
create_session_for_test <- function(vol_id = 1777, name = "delete_session test") {
  databraryr:::perform_api_post(
    path = sprintf(databraryr:::API_VOLUME_SESSIONS, vol_id),
    body = list(name = name),
    vb = FALSE
  )
}

test_that("delete_session deletes an existing session", {
  created <- create_session_for_test(name = "delete_session happy path")
  skip_if_null_response(created, "create session for delete_session happy path")

  session_id <- created$id
  result <- delete_session(vol_id = 1777, session_id = session_id, vb = FALSE)

  expect_true(result)

  # Verify it's gone
  expect_null(get_session_by_id(vol_id = 1777, session_id = session_id, vb = FALSE))
})

test_that("delete_session returns FALSE for non-existent session", {
  expect_false(delete_session(vol_id = 1777, session_id = 999999999, vb = FALSE))
})

test_that("delete_session works with verbose mode", {
  created <- create_session_for_test(name = "delete_session vb")
  skip_if_null_response(created, "create session for delete_session vb")

  expect_true(delete_session(vol_id = 1777, session_id = created$id, vb = TRUE))
})

test_that("delete_session works with custom request object", {
  created <- create_session_for_test(name = "delete_session custom rq")
  skip_if_null_response(created, "create session for delete_session custom rq")

  custom_rq <- databraryr::make_default_request()
  expect_true(
    delete_session(
      vol_id = 1777,
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
  expect_error(delete_session(vol_id = 1777, session_id = -1))
  expect_error(delete_session(vol_id = 1777, session_id = 0))
  expect_error(delete_session(vol_id = 1777, session_id = "1"))
  expect_error(delete_session(vol_id = 1777, session_id = TRUE))
  expect_error(delete_session(vol_id = 1777, session_id = list(a = 1)))
  expect_error(delete_session(vol_id = 1777, session_id = c(1, 2)))
  expect_error(delete_session(vol_id = 1777, session_id = 1.5))
  expect_error(delete_session(vol_id = 1777, session_id = NULL))
  expect_error(delete_session(vol_id = 1777, session_id = NA))
})

test_that("delete_session rejects invalid vb parameter", {
  expect_error(delete_session(vol_id = 1777, session_id = 1, vb = -1))
  expect_error(delete_session(vol_id = 1777, session_id = 1, vb = 3))
  expect_error(delete_session(vol_id = 1777, session_id = 1, vb = "a"))
  expect_error(delete_session(vol_id = 1777, session_id = 1, vb = list(a = 1)))
  expect_error(delete_session(vol_id = 1777, session_id = 1, vb = c(TRUE, FALSE)))
  expect_error(delete_session(vol_id = 1777, session_id = 1, vb = NULL))
})

test_that("delete_session rejects invalid rq parameter", {
  expect_error(delete_session(vol_id = 1777, session_id = 1, rq = "a"))
  expect_error(delete_session(vol_id = 1777, session_id = 1, rq = -1))
  expect_error(delete_session(vol_id = 1777, session_id = 1, rq = c(2, 3)))
  expect_error(delete_session(vol_id = 1777, session_id = 1, rq = list(a = 1)))
  expect_error(delete_session(vol_id = 1777, session_id = 1, rq = TRUE))
})
