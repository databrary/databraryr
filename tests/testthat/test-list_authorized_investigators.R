# list_authorized_investigators ---------------------------------------------------------
test_that("list_authorized_investigators returns investigators for institution 1", {
  login_test_account()
  result <- list_authorized_investigators(institution_id = 1)
  skip_if_null_response(result, "list_authorized_investigators(institution_id = 1)")
  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
  expect_true(all(c("institution_id", "user_id") %in% names(result)))
})

test_that("list_authorized_investigators rejects bad input parameters", {
  expect_error(list_authorized_investigators(institution_id = "a"))
  expect_error(list_authorized_investigators(institution_id = -1))
  expect_error(list_authorized_investigators(institution_id = TRUE))
  expect_error(list_authorized_investigators(institution_id = c(1, 3)))
  expect_error(list_authorized_investigators(institution_id = list(a = 1, b =
                                                               2)))
  
  expect_error(list_authorized_investigators(vb = "a"))
  expect_error(list_authorized_investigators(vb = -1))
  expect_error(list_authorized_investigators(vb = c(2, 3)))
  expect_error(list_authorized_investigators(vb = list(a = 1, b = 2)))
})

