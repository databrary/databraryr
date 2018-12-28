context("login and logout")
library(databraryapi)

# login_db -------------------------------------------------------------------
test_that("login_db rejects bad input parameters", {
  expect_error(login_db(vb = -1))
  expect_error(login_db(vb = 3))
  expect_error(login_db(vb = "a"))

  expect_error(login_db(email = -1))
  expect_error(login_db(email = c("a", "b")))
})

# logout_db ------------------------------------------------------------------
test_that("logout_db rejects bad input parameters", {
  expect_error(logout_db(vb = -1))
  expect_error(logout_db(vb = 3))
  expect_error(logout_db(vb = "a"))
})
