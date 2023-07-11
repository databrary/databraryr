# list_containers_records_json -----------------------------------------------
test_that("list_containers_records_json returns character", {
  expect_true(class(list_containers_records_json()) == "character")
})

test_that("list_containers_records_json is JSON", {
  expect_true(jsonlite::validate(list_containers_records_json()))
})

test_that("list_containers_records_json rejects bad input parameters", {
  expect_error(list_containers_records_json(vol_id = "a"))
  expect_error(list_containers_records_json(vol_id = c(1,2)))
  expect_error(list_containers_records_json(vol_id = TRUE))
  expect_error(list_containers_records_json(vol_id = list(a=1, b=2)))
  expect_error(list_containers_records_json(vol_id = -1))
  expect_error(list_containers_records_json(vol_id = 0))

  expect_error(list_containers_records_json(vb = -1))
  expect_error(list_containers_records_json(vb = 3))
  expect_error(list_containers_records_json(vb = "a"))
  #  expect_error(list_containers_records_json(vb = list(a=1, b=2)))
})
