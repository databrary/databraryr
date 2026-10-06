# list_session_assets ---------------------------------------------------------
test_that("list_session_assets requires volume id", {
  expect_error(list_session_assets(session_id = 9807))
})

test_that("list_session_assets returns tibble for accessible session", {
  login_test_account()
  result <- list_session_assets(session_id = 9, vol_id = 2)
  skip_if_null_response(result, "list_session_assets(session_id = 9, vol_id = 2)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_session_assets rejects bad input parameters", {
  expect_error(list_session_assets(session_id = "a", vol_id = 1))
  expect_error(list_session_assets(session_id = c(1, 2), vol_id = 1))
  expect_error(list_session_assets(session_id = TRUE, vol_id = 1))
  expect_error(list_session_assets(session_id = list(a = 1, b = 2), vol_id = 1))
  expect_error(list_session_assets(session_id = -1, vol_id = 1))

  expect_error(list_session_assets(session_id = 9, vol_id = "a"))
  expect_error(list_session_assets(session_id = 9, vol_id = c(1, 2)))
  expect_error(list_session_assets(session_id = 9, vol_id = TRUE))
  expect_error(list_session_assets(session_id = 9, vol_id = list(a = 1, b = 2)))
  expect_error(list_session_assets(session_id = 9, vol_id = -1))

  expect_error(list_session_assets(session_id = 9, vol_id = 1, vb = "a"))
  expect_error(list_session_assets(session_id = 9, vol_id = 1, vb = list(a = 1, b = 2)))

  expect_error(list_session_assets(session_id = 9, vol_id = 1, rq = "a"))
  expect_error(list_session_assets(session_id = 9, vol_id = 1, rq = -1))
  expect_error(list_session_assets(session_id = 9, vol_id = 1, rq = c(2, 3)))
  expect_error(list_session_assets(session_id = 9, vol_id = 1, rq = list(a = 1, b = 2)))
})
