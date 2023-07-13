# list_sessions --------------------------------------------------------------
test_that("list_sessions returns data.frame", {
  expect_true(class(list_sessions()) == "data.frame")
})

test_that("list_sessions rejects bad input parameters", {
  expect_error(list_sessions(vol_id = "a"))
  expect_error(list_sessions(vol_id = TRUE))
  expect_error(list_sessions(vol_id = list(a=1, b=2)))
  expect_error(list_sessions(vol_id = -1))
  
  expect_error(list_sessions(vb = -1))
  expect_error(list_sessions(vb = 3))
  expect_error(list_sessions(vb = "a"))
})
