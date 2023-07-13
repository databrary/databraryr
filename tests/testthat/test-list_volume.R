# list_volume ---------------------------------------------------------
test_that("list_volume returns a list or is NULL.", {
  expect_true((is.null(list_volume()) ||
                 (class(list_volume()) == "list")))
})

test_that("list_volume rejects bad input parameters", {
  expect_error(list_volume(vol_id = "a"))
  expect_error(list_volume(vol_id = -1))
  expect_error(list_volume(vol_id = TRUE))
  expect_error(list_volume(vol_id = c(1,3)))
  expect_error(list_volume(vol_id = list(a=1, b=2)))
  
  expect_error(list_volume(vb = "a"))
  expect_error(list_volume(vb = -1))
  expect_error(list_volume(vb = c(2,3)))
  expect_error(list_volume(vb = list(a=1, b=2)))
})
