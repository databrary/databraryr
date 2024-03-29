# list_institutional_sponsors ---------------------------------------------------------
test_that("list_institutional_sponsors returns a data.frame or is NULL.", {
  expect_true((is.null(list_institutional_sponsors()) ||
                 ("data.frame" %in% class(list_institutional_sponsors()))))
})

test_that("list_institutional_sponsors rejects bad input parameters", {
  expect_error(list_institutional_sponsors(party_id = "a"))
  expect_error(list_institutional_sponsors(party_id = -1))
  expect_error(list_institutional_sponsors(party_id = TRUE))
  expect_error(list_institutional_sponsors(party_id = c(1,3)))
  expect_error(list_institutional_sponsors(party_id = list(a=1, b=2)))
  
  expect_error(list_institutional_sponsors(report_target_party = "a"))
  expect_error(list_institutional_sponsors(report_target_party = -1))
  expect_error(list_institutional_sponsors(report_target_party = c(2,3)))
  expect_error(list_institutional_sponsors(report_target_party = list(a=1, b=2)))
  
  expect_error(list_institutional_sponsors(vb = "a"))
  expect_error(list_institutional_sponsors(vb = -1))
  expect_error(list_institutional_sponsors(vb = c(2,3)))
  expect_error(list_institutional_sponsors(vb = list(a=1, b=2)))
})
