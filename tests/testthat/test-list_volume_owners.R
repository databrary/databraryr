test_that("list_volume_owners returns data.frame", {
  expect_true("data.frame" %in% class(list_volume_owners()))
})

test_that("list_volume_owners returns NULL for volume 3", {
  expect_true(is.null(list_volume_owners(this_vol_id = 3)))
})

test_that("list_volume_owners rejects bad input parameters", {
  expect_error(list_volume_owners(this_vol_id = "a"))
  expect_error(list_volume_owners(this_vol_id = c(1,2)))
  expect_error(list_volume_owners(this_vol_id = TRUE))
  expect_error(list_volume_owners(this_vol_id = list(a=1, b=2)))
  expect_error(list_volume_owners(this_vol_id = -1))
  
  expect_error(list_volume_owners(vb = -1))
  expect_error(list_volume_owners(vb = 3))
  expect_error(list_volume_owners(vb = "a"))
  expect_error(list_volume_owners(vb = list(a=1, b=2)))
})
