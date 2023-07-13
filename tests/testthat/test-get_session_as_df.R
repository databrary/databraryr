# get_session_as_df ---------------------------------------------------------
test_that("get_session_as_df returns a data frame", {
  expect_true("data.frame" %in% class(get_session_as_df()))
})

test_that("get_session_as_df rejects bad input parameters", {
  expect_error(get_session_as_df(vol_id = -1))
  expect_error(get_session_as_df(vol_id = 0))
  expect_error(get_session_as_df(vol_id = "a"))
  expect_error(get_session_as_df(vol_id = list(a=1, b=2)))
  expect_error(get_session_as_df(vol_id = TRUE))
  
  expect_error(get_session_as_df(vb = -1))
  expect_error(get_session_as_df(vb = 3))
  expect_error(get_session_as_df(vb = "a"))
  expect_error(get_session_as_df(vb = list(a=1, b=2)))
})
