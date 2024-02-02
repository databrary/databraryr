# list_party ---------------------------------------------------------
test_that("list_party returns a list or is NULL.", {
  expect_true((is.null(list_party()) ||
                 ("list" %in% class(list_party()))))
})

test_that("list_party rejects bad input parameters", {
  expect_error(list_party(party_id = "a"))
  expect_error(list_party(party_id = -1))
  expect_error(list_party(party_id = TRUE))
  expect_error(list_party(party_id = c(1,3)))
  expect_error(list_party(party_id = list(a=1, b=2)))
  
  expect_error(list_party(component = -1))
  expect_error(list_party(component = TRUE))
  expect_error(list_party(component = c(1,3)))
  expect_error(list_party(component = list(a=1, b=2)))

  expect_error(list_party(vb = "a"))
  expect_error(list_party(vb = -1))
  expect_error(list_party(vb = c(2,3)))
  expect_error(list_party(vb = list(a=1, b=2)))
})
