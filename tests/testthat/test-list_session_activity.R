# list_session_activity ---------------------------------------------------------
test_that("list_session_activity returns tibble or is NULL", {
  login_test_account()
  result <- list_session_activity(vol_id = 1892, session_id = 76113)
  skip_if_null_response(result, "list_session_activity(vol_id = 1892, session_id = 76113)")
  expect_s3_class(result, "tbl_df")
})

test_that("list_session_activity rejects bad input parameters", {
  expect_error(list_session_activity(session_id = "a"))
  expect_error(list_session_activity(session_id = c(1, 2)))
  expect_error(list_session_activity(session_id = TRUE))
  expect_error(list_session_activity(session_id = list(a = 1, b = 2)))
  expect_error(list_session_activity(session_id = -1))
  
  expect_error(list_session_activity(vb = -1))
  expect_error(list_session_activity(vb = 3))
  expect_error(list_session_activity(vb = "a"))
  expect_error(list_session_activity(vb = list(a = 1, b = 2)))
  
  expect_error(list_session_activity(rq = "a"))
  expect_error(list_session_activity(rq = -1))
  expect_error(list_session_activity(rq = c(2, 3)))
  expect_error(list_session_activity(rq = list(a = 1, b = 2)))
})
