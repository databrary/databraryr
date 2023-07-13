# list_sessions_in_volume --------------------------------------------------------------
test_that("list_sessions_in_volume returns data.frame", {
  expect_true(class(list_sessions_in_volume()) == "data.frame")
})

test_that("list_sessions_in_volume rejects bad input parameters", {
  expect_error(list_sessions_in_volume(vol_id = "a"))
  expect_error(list_sessions_in_volume(vol_id = TRUE))
  expect_error(list_sessions_in_volume(vol_id = list(a=1, b=2)))
  expect_error(list_sessions_in_volume(vol_id = -1))
  
  expect_error(list_sessions_in_volume(vb = -1))
  expect_error(list_sessions_in_volume(vb = 3))
  expect_error(list_sessions_in_volume(vb = "a"))
})