# get_session_by_name ---------------------------------------------------------
test_that("get_session_by_name returns session metadata", {
  login_test_account()
  result <- get_session_by_name("to-airport", vol_id = 2)
  skip_if_null_response(result, "get_session_by_name(\"to-airport\", vol_id = 2)")
  expect_true(is.list(result))
  expect_equal(length(result), 1)
  expect_equal(result[[1]]$id, 11)
})

test_that("get_session_by_name rejects bad input parameters", {
  expect_error(get_session_by_name(session_name = 123))
  expect_error(get_session_by_name(session_name = c("a", "b")))
  expect_error(get_session_by_name(session_name = NA_character_))
  
  expect_error(get_session_by_name(vol_id = -1))
  expect_error(get_session_by_name(vol_id = "a"))
  expect_error(get_session_by_name(vol_id = TRUE))
  
  expect_error(get_session_by_name(vb = "a"))
  expect_error(get_session_by_name(vb = -1))
  expect_error(get_session_by_name(vb = c(2,3)))
  
  expect_error(get_session_by_name(rq = "a"))
  expect_error(get_session_by_name(rq = -1))
  expect_error(get_session_by_name(rq = c(2,3)))
  expect_error(get_session_by_name(rq = list(a=1, b=2)))
})