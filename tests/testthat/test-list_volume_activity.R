# list_volume_activity ---------------------------------------------------------
test_that("list_volume_activity returns data.frame or is NULL", {
  expect_true((is.null(list_volume_activity()) ||
                 (class(list_volume_activity()) == "data.frame")))
})

test_that("list_volume_activity rejects bad input parameters", {
  expect_error(list_volume_activity(vol_id = "a"))
  expect_error(list_volume_activity(vol_id = c(1,2)))
  expect_error(list_volume_activity(vol_id = TRUE))
  expect_error(list_volume_activity(vol_id = list(a=1, b=2)))
  expect_error(list_volume_activity(vol_id = -1))
  
  expect_error(list_volume_activity(vb = -1))
  expect_error(list_volume_activity(vb = 3))
  expect_error(list_volume_activity(vb = "a"))
  expect_error(list_volume_activity(vb = list(a=1, b=2)))
})