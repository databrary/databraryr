# create_folder() --------------------------------------------------------------
login_test_account()

test_that("create_folder creates a folder with name only", {
  result <- create_folder(vol_id = 1777, name = "Test folder", vb = FALSE)
  skip_if_null_response(result, "create_folder(vol_id = 1777, name = 'Test folder')")

  if (!is.null(result$id)) {
    delete_folder(vol_id = 1777, folder_id = result$id, vb = FALSE)
  }

  expect_type(result, "list")
  expect_true(!is.null(result$id))
  expect_true(is.numeric(result$id) || is.integer(result$id))
  expect_true(result$id > 0)
  expect_equal(as.integer(result$volume), 1777L)
  expect_equal(result$name, "Test folder")
})

test_that("create_folder accepts a Date source_date", {
  result <- create_folder(
    vol_id = 1777,
    name = "Test folder with Date",
    source_date = as.Date("2024-03-15"),
    vb = FALSE
  )
  skip_if_null_response(result, "create_folder with Date source_date")

  if (!is.null(result$id)) {
    delete_folder(vol_id = 1777, folder_id = result$id, vb = FALSE)
  }

  expect_type(result, "list")
  expect_true(!is.null(result$id))
})

test_that("create_folder accepts an ISO string source_date", {
  result <- create_folder(
    vol_id = 1777,
    name = "Test folder with ISO date",
    source_date = "2024-03-15",
    vb = FALSE
  )
  skip_if_null_response(result, "create_folder with ISO source_date")

  if (!is.null(result$id)) {
    delete_folder(vol_id = 1777, folder_id = result$id, vb = FALSE)
  }

  expect_type(result, "list")
})

test_that("create_folder works with verbose mode", {
  result <- create_folder(
    vol_id = 1777,
    name = "Test folder vb",
    vb = TRUE
  )
  skip_if_null_response(result, "create_folder with vb = TRUE")

  if (!is.null(result$id)) {
    delete_folder(vol_id = 1777, folder_id = result$id, vb = FALSE)
  }

  expect_type(result, "list")
})

test_that("create_folder works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- create_folder(
    vol_id = 1777,
    name = "Test folder custom rq",
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "create_folder with custom_rq")

  if (!is.null(result$id)) {
    delete_folder(vol_id = 1777, folder_id = result$id, vb = FALSE)
  }

  expect_type(result, "list")
})

test_that("create_folder returns NULL for non-existent volume", {
  expect_null(create_folder(vol_id = 999999999, name = "Test", vb = FALSE))
})

test_that("create_folder rejects invalid name", {
  expect_error(create_folder(vol_id = 1777, name = ""))
  expect_error(create_folder(vol_id = 1777, name = "   "))
  expect_error(create_folder(vol_id = 1777, name = 123))
  expect_error(create_folder(vol_id = 1777, name = c("A", "B")))
  expect_error(create_folder(vol_id = 1777, name = NULL))
  expect_error(create_folder(vol_id = 1777, name = NA))
})

test_that("create_folder rejects malformed source_date", {
  expect_error(create_folder(vol_id = 1777, name = "Test", source_date = "not-a-date"))
  expect_error(create_folder(vol_id = 1777, name = "Test", source_date = ""))
  expect_error(create_folder(vol_id = 1777, name = "Test", source_date = 123))
  expect_error(
    create_folder(
      vol_id = 1777,
      name = "Test",
      source_date = as.Date(c("2024-01-01", "2024-02-01"))
    )
  )
})

test_that("create_folder rejects invalid release_level", {
  expect_error(create_folder(vol_id = 1777, name = "Test", release_level = ""))
  expect_error(create_folder(vol_id = 1777, name = "Test", release_level = 1))
  expect_error(create_folder(vol_id = 1777, name = "Test", release_level = c("A", "B")))
})

test_that("create_folder rejects invalid vol_id", {
  expect_error(create_folder(vol_id = -1, name = "Test"))
  expect_error(create_folder(vol_id = 0, name = "Test"))
  expect_error(create_folder(vol_id = "1", name = "Test"))
  expect_error(create_folder(vol_id = TRUE, name = "Test"))
  expect_error(create_folder(vol_id = list(a = 1), name = "Test"))
  expect_error(create_folder(vol_id = c(1, 2), name = "Test"))
  expect_error(create_folder(vol_id = 1.5, name = "Test"))
})

test_that("create_folder rejects invalid vb parameter", {
  expect_error(create_folder(vol_id = 1777, name = "Test", vb = -1))
  expect_error(create_folder(vol_id = 1777, name = "Test", vb = "a"))
  expect_error(create_folder(vol_id = 1777, name = "Test", vb = list(a = 1)))
  expect_error(create_folder(vol_id = 1777, name = "Test", vb = c(TRUE, FALSE)))
  expect_error(create_folder(vol_id = 1777, name = "Test", vb = NULL))
})

test_that("create_folder rejects invalid rq parameter", {
  expect_error(create_folder(vol_id = 1777, name = "Test", rq = "a"))
  expect_error(create_folder(vol_id = 1777, name = "Test", rq = -1))
  expect_error(create_folder(vol_id = 1777, name = "Test", rq = c(2, 3)))
  expect_error(create_folder(vol_id = 1777, name = "Test", rq = list(a = 1)))
  expect_error(create_folder(vol_id = 1777, name = "Test", rq = TRUE))
})
