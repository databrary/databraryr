# get_db_stats ---------------------------------------------------------
test_that("get_db_stats returns statistics snapshot", {
  login_test_account()
  stats <- get_db_stats()
  skip_if_null_response(stats, "get_db_stats()")
  expect_s3_class(stats, "tbl_df")
  expect_true("date" %in% names(stats))
})

test_that("get_db_stats returns data.frames for supported types", {
  login_test_account()
  types <- c("people", "institutions", "places", "datasets", "data", "volumes", "numbers")
  for (type in types) {
    result <- get_db_stats(type)
    skip_if_null_response(result, sprintf("get_db_stats('%s')", type))
    expect_s3_class(result, "tbl_df")
  }

  stats_tbl <- get_db_stats("stats")
  skip_if_null_response(stats_tbl, "get_db_stats('stats')")
  expect_s3_class(stats_tbl, "tbl_df")
  expect_true("date" %in% names(stats_tbl))
})

test_that("get_db_stats rejects bad input parameters", {
  expect_error(get_db_stats(type = "a"))
  expect_error(get_db_stats(type = -1))
  expect_error(get_db_stats(type = c(1, 2)))
  
  expect_error(get_db_stats(vb = -1))
  expect_error(get_db_stats(vb = 3))
  expect_error(get_db_stats(vb = "a"))
  expect_error(get_db_stats(vb = list(a = 1, b = 2)))
})
