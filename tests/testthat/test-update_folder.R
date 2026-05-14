# update_folder() --------------------------------------------------------------
login_test_account()

test_that("update_folder replaces name via PUT", {
  fid <- make_test_folder("update_folder original")
  skip_if_null_response(fid, "create_folder for update_folder name test")

  result <- update_folder(
    vol_id = TEST_VOL_ID,
    folder_id = fid,
    name = "update_folder replaced",
    vb = FALSE
  )
  skip_if_null_response(result, "update_folder(name=...)")

  expect_type(result, "list")
  expect_equal(result$name, "update_folder replaced")
  expect_equal(as.integer(result$id), as.integer(fid))
})

test_that("update_folder replaces source_date with a Date object", {
  fid <- make_test_folder("update_folder source_date Date")
  skip_if_null_response(fid, "create_folder for update_folder source_date Date")

  result <- update_folder(
    vol_id = TEST_VOL_ID,
    folder_id = fid,
    name = "update_folder source_date Date",
    source_date = as.Date("2024-03-15"),
    vb = FALSE
  )
  skip_if_null_response(result, "update_folder(source_date=Date)")
  expect_type(result, "list")
})

test_that("update_folder returns NULL for non-existent folder", {
  result <- update_folder(
    vol_id = TEST_VOL_ID,
    folder_id = TEST_MISSING_ID,
    name = "nope",
    vb = FALSE
  )
  expect_null(result)
})

test_that("update_folder works with verbose mode", {
  fid <- make_test_folder("update_folder vb")
  skip_if_null_response(fid, "create_folder for update_folder vb")

  result <- update_folder(
    vol_id = TEST_VOL_ID,
    folder_id = fid,
    name = "update_folder vb replaced",
    vb = TRUE
  )
  skip_if_null_response(result, "update_folder vb")
  expect_type(result, "list")
})

test_that("update_folder works with custom request object", {
  fid <- make_test_folder("update_folder custom rq")
  skip_if_null_response(fid, "create_folder for update_folder custom rq")

  custom_rq <- databraryr::make_default_request()
  result <- update_folder(
    vol_id = TEST_VOL_ID,
    folder_id = fid,
    name = "update_folder custom rq replaced",
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "update_folder custom rq")
  expect_type(result, "list")
})

test_that("update_folder rejects missing/invalid name", {
  expect_error(update_folder(vol_id = 1, folder_id = 1))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = ""))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "   "))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = 123))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = c("A", "B")))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = NULL))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = NA))
})

test_that("update_folder rejects malformed source_date", {
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", source_date = "not-a-date"))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", source_date = ""))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", source_date = 123))
})

test_that("update_folder rejects invalid release_level", {
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", release_level = ""))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", release_level = 1))
})

test_that("update_folder rejects invalid vol_id", {
  expect_error(update_folder(vol_id = -1, folder_id = 1, name = "x"))
  expect_error(update_folder(vol_id = 0, folder_id = 1, name = "x"))
  expect_error(update_folder(vol_id = "1", folder_id = 1, name = "x"))
  expect_error(update_folder(vol_id = TRUE, folder_id = 1, name = "x"))
  expect_error(update_folder(vol_id = c(1, 2), folder_id = 1, name = "x"))
  expect_error(update_folder(vol_id = 1.5, folder_id = 1, name = "x"))
})

test_that("update_folder rejects invalid folder_id", {
  expect_error(update_folder(vol_id = 1, folder_id = -1, name = "x"))
  expect_error(update_folder(vol_id = 1, folder_id = 0, name = "x"))
  expect_error(update_folder(vol_id = 1, folder_id = "1", name = "x"))
  expect_error(update_folder(vol_id = 1, folder_id = TRUE, name = "x"))
  expect_error(update_folder(vol_id = 1, folder_id = c(1, 2), name = "x"))
  expect_error(update_folder(vol_id = 1, folder_id = 1.5, name = "x"))
})

test_that("update_folder rejects invalid vb parameter", {
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", vb = -1))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", vb = "a"))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", vb = c(TRUE, FALSE)))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", vb = NULL))
})

test_that("update_folder rejects invalid rq parameter", {
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", rq = "a"))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", rq = -1))
  expect_error(update_folder(vol_id = 1, folder_id = 1, name = "x", rq = TRUE))
})
