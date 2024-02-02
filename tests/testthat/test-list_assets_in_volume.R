# list_assets_in_volume -----------------------------------------------
test_that("list_assets_in_volume returns a data frame or is NULL.", {
  expect_true((is.null(list_assets_in_volume()) ||
                 ("data.frame" %in% class(list_assets_in_volume()))))
})

test_that("list_assets_in_volume rejects bad input parameters", {
  expect_error(list_assets_in_volume(vol_id = -1))
  expect_error(list_assets_in_volume(vol_id = 0))
  expect_error(list_assets_in_volume(vol_id = "a"))
  expect_error(list_assets_in_volume(vol_id = list(a=1, b=2)))
  expect_error(list_assets_in_volume(vol_id = TRUE))
  
  expect_error(list_assets_in_volume(vb = -1))
  expect_error(list_assets_in_volume(vb = 3))
  expect_error(list_assets_in_volume(vb = "a"))
  expect_error(list_assets_in_volume(vb = list(a=1, b=2)))
})
