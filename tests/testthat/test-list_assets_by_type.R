# list_assets_by_type ---------------------------------------------------
test_that("list_assets_by_type returns data.frame", {
  expect_true(class(list_assets_by_type()) == "data.frame")
})

test_that("list_assets_by_type rejects bad input parameters", {
  expect_error(list_assets_by_type(vol_id = -1))
  expect_error(list_assets_by_type(vol_id = 0))
  expect_error(list_assets_by_type(vol_id = "a"))
  expect_error(list_assets_by_type(vol_id = list(a=1, b=2)))
  expect_error(list_assets_by_type(vol_id = TRUE))
  
  expect_error(list_assets_by_type(type = "a"))
  expect_error(list_assets_by_type(type = c(1,2)))
  expect_error(list_assets_by_type(type = TRUE))
  expect_error(list_assets_by_type(type = list(a=1, b=2)))
  
  expect_error(list_assets_by_type(vb = -1))
  expect_error(list_assets_by_type(vb = 3))
  expect_error(list_assets_by_type(vb = "a"))
  expect_error(list_assets_by_type(vb = list(a=1, b=2)))
})
