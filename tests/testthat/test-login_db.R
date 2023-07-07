test_that("login_db rejects bad input parameters", {
  expect_error(login_db(email = -1))
  expect_error(login_db(email = c("a", "b")))
  
  expect_error(login_db(login_url = -1))
  expect_error(login_db(login_url = c("a", "b")))
  expect_error(login_db(login_url = TRUE))
  
  expect_error(login_db(return_response = -1))
  expect_error(login_db(return_response = 3))
  expect_error(login_db(return_response = "a"))

  expect_error(login_db(save_session = -1))
  expect_error(login_db(save_session = 3))
  expect_error(login_db(save_session = "a"))

  expect_error(login_db(stored_credentials = -1))
  expect_error(login_db(stored_credentials = 3))
  expect_error(login_db(stored_credentials = "a"))
  
  expect_error(login_db(system_credentials = -1))
  expect_error(login_db(system_credentials = 3))
  expect_error(login_db(system_credentials = "a"))
  
  expect_error(login_db(credentials_file = -1))
  expect_error(login_db(credentials_file = 3))
  expect_error(login_db(credentials_file = TRUE))

  expect_error(login_db(vb = -1))
  expect_error(login_db(vb = 3))
  expect_error(login_db(vb = "a"))
})
