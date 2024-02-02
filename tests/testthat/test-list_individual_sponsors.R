# list_individual_sponsors ---------------------------------------------------------
test_that("list_individual_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(list_individual_sponsors()) ||
                 ("data.frame" %in% class(list_individual_sponsors()))))
})

test_that("list_individual_sponsors rejects bad input parameters", {
  expect_error(list_individual_sponsors(party_id = "a"))
  expect_error(list_individual_sponsors(party_id = -1))
  expect_error(list_individual_sponsors(party_id = TRUE))
  expect_error(list_individual_sponsors(party_id = c(1,3)))
  expect_error(list_individual_sponsors(party_id = list(a=1, b=2)))

  expect_error(list_individual_sponsors(vb = "a"))
  expect_error(list_individual_sponsors(vb = -1))
  expect_error(list_individual_sponsors(vb = c(2,3)))
  expect_error(list_individual_sponsors(vb = list(a=1, b=2)))
})