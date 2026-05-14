# delete_folder() --------------------------------------------------------------
login_test_account()

test_that("delete_folder deletes an existing folder", {
  created <- create_folder(vol_id = 1777, name = "delete_folder happy path", vb = FALSE)
  skip_if_null_response(created, "create_folder for delete_folder happy path")

  folder_id <- created$id
  result <- delete_folder(vol_id = 1777, folder_id = folder_id, vb = FALSE)

  expect_true(result)

  # Verify it's gone
  expect_null(get_folder_by_id(folder_id = folder_id, vol_id = 1777, vb = FALSE))
})

test_that("delete_folder returns FALSE for non-existent folder", {
  expect_false(delete_folder(vol_id = 1777, folder_id = 999999999, vb = FALSE))
})

test_that("delete_folder works with verbose mode", {
  created <- create_folder(vol_id = 1777, name = "delete_folder vb", vb = FALSE)
  skip_if_null_response(created, "create_folder for delete_folder vb")

  expect_true(delete_folder(vol_id = 1777, folder_id = created$id, vb = TRUE))
})

test_that("delete_folder works with custom request object", {
  created <- create_folder(
    vol_id = 1777,
    name = "delete_folder custom rq",
    vb = FALSE
  )
  skip_if_null_response(created, "create_folder for delete_folder custom rq")

  custom_rq <- databraryr::make_default_request()
  expect_true(
    delete_folder(
      vol_id = 1777,
      folder_id = created$id,
      rq = custom_rq,
      vb = FALSE
    )
  )
})

test_that("delete_folder rejects invalid vol_id", {
  expect_error(delete_folder(vol_id = -1, folder_id = 1))
  expect_error(delete_folder(vol_id = 0, folder_id = 1))
  expect_error(delete_folder(vol_id = "1", folder_id = 1))
  expect_error(delete_folder(vol_id = TRUE, folder_id = 1))
  expect_error(delete_folder(vol_id = list(a = 1), folder_id = 1))
  expect_error(delete_folder(vol_id = c(1, 2), folder_id = 1))
  expect_error(delete_folder(vol_id = 1.5, folder_id = 1))
  expect_error(delete_folder(vol_id = NULL, folder_id = 1))
  expect_error(delete_folder(vol_id = NA, folder_id = 1))
})

test_that("delete_folder rejects invalid folder_id", {
  expect_error(delete_folder(vol_id = 1777, folder_id = -1))
  expect_error(delete_folder(vol_id = 1777, folder_id = 0))
  expect_error(delete_folder(vol_id = 1777, folder_id = "1"))
  expect_error(delete_folder(vol_id = 1777, folder_id = TRUE))
  expect_error(delete_folder(vol_id = 1777, folder_id = list(a = 1)))
  expect_error(delete_folder(vol_id = 1777, folder_id = c(1, 2)))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1.5))
  expect_error(delete_folder(vol_id = 1777, folder_id = NULL))
  expect_error(delete_folder(vol_id = 1777, folder_id = NA))
})

test_that("delete_folder rejects invalid vb parameter", {
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, vb = -1))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, vb = 3))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, vb = "a"))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, vb = list(a = 1)))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, vb = c(TRUE, FALSE)))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, vb = NULL))
})

test_that("delete_folder rejects invalid rq parameter", {
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, rq = "a"))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, rq = -1))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, rq = c(2, 3)))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, rq = list(a = 1)))
  expect_error(delete_folder(vol_id = 1777, folder_id = 1, rq = TRUE))
})
