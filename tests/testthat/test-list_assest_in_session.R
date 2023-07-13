# list_assets_in_session -----------------------------------------------
test_that("list_assets_by_type returns data.frame", {
  expect_true(class(list_assets_in_session()) == "data.frame")
})

test_that("list_assets_in_session rejects bad input parameters", {
  expect_error(list_assets_in_session(vol_id = -1))
  expect_error(list_assets_in_session(vol_id = 0))
  expect_error(list_assets_in_session(vol_id = "a"))
  expect_error(list_assets_in_session(vol_id = list(a=1, b=2)))
  expect_error(list_assets_in_session(vol_id = TRUE))
  
  expect_error(list_assets_in_session(session_id = -1))
  expect_error(list_assets_in_session(session_id = 0))
  expect_error(list_assets_in_session(session_id = "a"))
  expect_error(list_assets_in_session(session_id = list(a=1, b=2)))
  expect_error(list_assets_in_session(session_id = TRUE))
  
  expect_error(list_assets_in_session(vb = -1))
  expect_error(list_assets_in_session(vb = 3))
  expect_error(list_assets_in_session(vb = "a"))
  expect_error(list_assets_in_session(vb = list(a=-1, b=2)))
})
