# get_party_as_df ---------------------------------------------------------
test_that("get_party_as_df returns a data frame", {
  expect_true(class(get_party_as_df()) == "data.frame")
  expect_true(length(get_party_as_df()) == 6)
})

test_that("get_permission_levels rejects bad input parameters", {
  expect_error(get_party_as_df(party_id = "a"))
  expect_error(get_party_as_df(party_id = -1))
  expect_error(get_party_as_df(party_id = c(2,3)))
  expect_error(get_party_as_df(party_id = TRUE))

  expect_error(get_party_as_df(convert_JSON = "a"))
  expect_error(get_party_as_df(convert_JSON = -1))
  expect_error(get_party_as_df(convert_JSON = c(2,3)))
  
  expect_error(get_party_as_df(vb = "a"))
  expect_error(get_party_as_df(vb = -1))
  expect_error(get_party_as_df(vb = c(2,3)))
})
