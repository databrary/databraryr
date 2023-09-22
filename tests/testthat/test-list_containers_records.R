# list_containers_records-----------------------------------------------
test_that("list_containers_records returns data.frame", {
  expect_true("list" %in% class(list_containers_records()))
})

test_that("list_assets_in_volume rejects bad input parameters", {
  expect_error(list_containers_records(vol_id = -1))
  expect_error(list_containers_records(vol_id = 0))
  expect_error(list_containers_records(vol_id = "a"))
  expect_error(list_containers_records(vol_id = list(a=1, b=2)))
  expect_error(list_containers_records(vol_id = TRUE))
  
  expect_error(list_containers_records(vb = -1))
  expect_error(list_containers_records(vb = 3))
  expect_error(list_containers_records(vb = "a"))
  expect_error(list_containers_records(vb = list(a=1, b=2)))
})
