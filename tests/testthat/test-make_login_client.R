test_that("make_login_client rejects bad input parameters", {
  expect_error(make_login_client(email = -1, password = "pw"))
  expect_error(make_login_client(email = c("a", "b"), password = "pw"))
  expect_error(make_login_client(email = list("a", "b"), password = "pw"))
  expect_error(make_login_client(email = TRUE, password = "pw"))

  expect_error(make_login_client(password = -1, email = "user@example.com"))
  expect_error(make_login_client(password = 3, email = "user@example.com"))
  expect_error(make_login_client(password = list("a", "b"), email = "user@example.com"))
  expect_error(make_login_client(password = TRUE, email = "user@example.com"))

  expect_error(make_login_client(store = -1, email = "user@example.com", password = "pw"))
  expect_error(make_login_client(store = "a", email = "user@example.com", password = "pw"))
  expect_error(make_login_client(store = list("a", "b"), email = "user@example.com", password = "pw"))

  expect_error(make_login_client(overwrite = -1, email = "user@example.com", password = "pw"))
  expect_error(make_login_client(overwrite = "a", email = "user@example.com", password = "pw"))
  expect_error(make_login_client(overwrite = list("a", "b"), email = "user@example.com", password = "pw"))

  expect_error(make_login_client(vb = -1))
  expect_error(make_login_client(vb = 3))
  expect_error(make_login_client(vb = "a"))

  expect_error(make_login_client(service = -1, email = "user@example.com", password = "pw"))
  expect_error(make_login_client(service = TRUE, email = "user@example.com", password = "pw"))
  expect_error(make_login_client(service = list("a", "b"), email = "user@example.com", password = "pw"))

  expect_error(make_login_client(rq = 3, email = "user@example.com", password = "pw"))
  expect_error(make_login_client(rq = "a", email = "user@example.com", password = "pw"))
  expect_error(make_login_client(rq = TRUE, email = "user@example.com", password = "pw"))
})