# list_affiliates ---------------------------------------------------------
test_that("list_affiliates returns a data.frame or is NULL.", {
  expect_true((is.null(list_affiliates()) ||
                 (class(list_affiliates()) == "data.frame")))
})

test_that("list_affiliates rejects bad input parameters", {
  expect_error(list_affiliates(party_id = "a"))
  expect_error(list_affiliates(party_id = -1))
  expect_error(list_affiliates(party_id = TRUE))
  expect_error(list_affiliates(party_id = c(1,3)))
  expect_error(list_affiliates(party_id = list(a=1, b=2)))
  
  expect_error(list_affiliates(vb = "a"))
  expect_error(list_affiliates(vb = -1))
  expect_error(list_affiliates(vb = c(2,3)))
  expect_error(list_affiliates(vb = list(a=1, b=2)))
})
