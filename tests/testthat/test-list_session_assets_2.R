# list_session_assets_2 ---------------------------------------------------------
test_that("list_session_assets_2 returns data.frame or is NULL", {
  expect_true((is.null(list_session_assets_2()) ||
                 ("data.frame" %in% class(list_session_assets_2()))))
})

test_that("list_session_assets_2 rejects bad input parameters", {
  expect_error(list_session_assets_2(session_id = "a"))
  expect_error(list_session_assets_2(session_id = c(1,2)))
  expect_error(list_session_assets_2(session_id = TRUE))
  expect_error(list_session_assets_2(session_id = list(a=1, b=2)))
  expect_error(list_session_assets_2(session_id = -1))
  
  expect_error(list_session_assets_2(vb = -1))
  expect_error(list_session_assets_2(vb = 3))
  expect_error(list_session_assets_2(vb = "a"))
  expect_error(list_session_assets_2(vb = list(a=1, b=2)))
  
  expect_error(list_session_assets_2(rq = "a"))
  expect_error(list_session_assets_2(rq = -1))
  expect_error(list_session_assets_2(rq = c(2,3)))
  expect_error(list_session_assets_2(rq = list(a=1, b=2)))
})
