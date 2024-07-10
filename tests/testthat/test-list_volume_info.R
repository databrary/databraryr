# list_volume_info --------------------------------------------------------------
test_that("list_volume_info returns data.frame given valid vol_id", {
  expect_true("data.frame" %in% class(list_volume_info()))
})

test_that("list_volume_info returns NULL given a non-shared vol_id", {
  expect_true(is.null(list_volume_info(vol_id = 237)))
})

test_that("list_volume_info rejects bad input parameters", {
  expect_error(list_volume_info(vol_id = "a"))
  expect_error(list_volume_info(vol_id = TRUE))
  expect_error(list_volume_info(vol_id = -1))

  expect_error(list_volume_info(vb = -1))
  expect_error(list_volume_info(vb = 3))
  expect_error(list_volume_info(vb = "a"))
  
  expect_error(list_volume_info(rq = -1))
  expect_error(list_volume_info(rq = TRUE))
  expect_error(list_volume_info(rq = c("a", "b")))
  expect_error(list_volume_info(rq = list("a" = 1, "b" = 2)))
})
