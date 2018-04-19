context("login and logout")
library(databraryapi)

test_that("login_db returns logical", {
  expect_true(class(login_db("some@email.com")) == "logical")
})

# test_that("login_db rejects bad input for vb", {
#   expect_error(login_db(vb = -1))
#   expect_error(login_db(vb = 3))
#   expect_error(login_db(vb = "a"))
# })
#
# test_that("login_db rejects bad input for email", {
#   expect_error(login_db(email = -1))
#   expect_error(login_db(email = c("a", "b")))
# })

# logout_db

test_that("logout_db returns logical", {
  expect_true(class(logout_db()) == "logical")
})

test_that("logout_db rejects bad input for vb", {
  expect_error(logout_db(vb = -1))
  expect_error(logout_db(vb = 3))
  expect_error(logout_db(vb = "a"))
})
