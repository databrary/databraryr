# patch_folder() ---------------------------------------------------------------
login_test_account()

new_folder_id <- function(name = "patch_folder test") {
  created <- create_folder(vol_id = 1777, name = name, vb = FALSE)
  if (is.null(created)) {
    return(NULL)
  }
  created$id
}

test_that("patch_folder updates name", {
  fid <- new_folder_id("patch_folder original")
  skip_if_null_response(fid, "create_folder for patch_folder name test")
  on.exit(delete_folder(vol_id = 1777, folder_id = fid, vb = FALSE), add = TRUE)

  result <- patch_folder(
    vol_id = 1777,
    folder_id = fid,
    name = "patch_folder renamed",
    vb = FALSE
  )
  skip_if_null_response(result, "patch_folder(name=...)")

  expect_type(result, "list")
  expect_equal(result$name, "patch_folder renamed")
  expect_equal(as.integer(result$id), as.integer(fid))
})

test_that("patch_folder updates source_date with a Date object", {
  fid <- new_folder_id("patch_folder source_date Date")
  skip_if_null_response(fid, "create_folder for patch_folder source_date Date")
  on.exit(delete_folder(vol_id = 1777, folder_id = fid, vb = FALSE), add = TRUE)

  result <- patch_folder(
    vol_id = 1777,
    folder_id = fid,
    source_date = as.Date("2024-03-15"),
    vb = FALSE
  )
  skip_if_null_response(result, "patch_folder(source_date=Date)")
  expect_type(result, "list")
})

test_that("patch_folder returns NULL when no fields provided", {
  expect_null(patch_folder(vol_id = 1777, folder_id = 1, vb = FALSE))
})

test_that("patch_folder returns NULL for non-existent folder", {
  result <- patch_folder(
    vol_id = 1777,
    folder_id = 999999999,
    name = "nope",
    vb = FALSE
  )
  expect_null(result)
})

test_that("patch_folder works with verbose mode", {
  fid <- new_folder_id("patch_folder vb")
  skip_if_null_response(fid, "create_folder for patch_folder vb")
  on.exit(delete_folder(vol_id = 1777, folder_id = fid, vb = FALSE), add = TRUE)

  result <- patch_folder(
    vol_id = 1777,
    folder_id = fid,
    name = "patch_folder vb renamed",
    vb = TRUE
  )
  skip_if_null_response(result, "patch_folder vb")
  expect_type(result, "list")
})

test_that("patch_folder works with custom request object", {
  fid <- new_folder_id("patch_folder custom rq")
  skip_if_null_response(fid, "create_folder for patch_folder custom rq")
  on.exit(delete_folder(vol_id = 1777, folder_id = fid, vb = FALSE), add = TRUE)

  custom_rq <- databraryr::make_default_request()
  result <- patch_folder(
    vol_id = 1777,
    folder_id = fid,
    name = "patch_folder custom rq renamed",
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "patch_folder custom rq")
  expect_type(result, "list")
})

test_that("patch_folder rejects invalid name", {
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = ""))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "   "))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = 123))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = c("A", "B")))
})

test_that("patch_folder rejects malformed source_date", {
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, source_date = "not-a-date"))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, source_date = ""))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, source_date = 123))
})

test_that("patch_folder rejects invalid release_level", {
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, release_level = ""))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, release_level = 1))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, release_level = c("A", "B")))
})

test_that("patch_folder rejects invalid vol_id", {
  expect_error(patch_folder(vol_id = -1, folder_id = 1, name = "x"))
  expect_error(patch_folder(vol_id = 0, folder_id = 1, name = "x"))
  expect_error(patch_folder(vol_id = "1", folder_id = 1, name = "x"))
  expect_error(patch_folder(vol_id = TRUE, folder_id = 1, name = "x"))
  expect_error(patch_folder(vol_id = c(1, 2), folder_id = 1, name = "x"))
  expect_error(patch_folder(vol_id = 1.5, folder_id = 1, name = "x"))
})

test_that("patch_folder rejects invalid folder_id", {
  expect_error(patch_folder(vol_id = 1777, folder_id = -1, name = "x"))
  expect_error(patch_folder(vol_id = 1777, folder_id = 0, name = "x"))
  expect_error(patch_folder(vol_id = 1777, folder_id = "1", name = "x"))
  expect_error(patch_folder(vol_id = 1777, folder_id = TRUE, name = "x"))
  expect_error(patch_folder(vol_id = 1777, folder_id = c(1, 2), name = "x"))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1.5, name = "x"))
})

test_that("patch_folder rejects invalid vb parameter", {
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "x", vb = -1))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "x", vb = "a"))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "x", vb = c(TRUE, FALSE)))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "x", vb = NULL))
})

test_that("patch_folder rejects invalid rq parameter", {
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "x", rq = "a"))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "x", rq = -1))
  expect_error(patch_folder(vol_id = 1777, folder_id = 1, name = "x", rq = TRUE))
})
