# check_duplicate_files_in_folder() --------------------------------------------
login_test_account()

TEST_VOL <- 1777

new_folder_id <- function(name = "check_duplicate_files_in_folder test") {
  created <- create_folder(vol_id = TEST_VOL, name = name, vb = FALSE)
  if (is.null(created)) {
    return(NULL)
  }
  created$id
}

test_that("check_duplicate_files_in_folder returns a tibble for an empty folder", {
  fid <- new_folder_id("check_duplicate_files_in_folder happy path")
  skip_if_null_response(fid, "create_folder for check_duplicate_files happy path")
  on.exit(delete_folder(vol_id = TEST_VOL, folder_id = fid, vb = FALSE), add = TRUE)

  filenames <- c("nonexistent_a.mp4", "nonexistent_b.mp4")
  result <- check_duplicate_files_in_folder(
    vol_id = TEST_VOL,
    folder_id = fid,
    filenames = filenames,
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_folder(empty folder)")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("filename", "exists"))
  expect_equal(nrow(result), length(filenames))
  expect_equal(result$filename, filenames)
  expect_type(result$exists, "logical")
  expect_true(all(!result$exists))
})

test_that("check_duplicate_files_in_folder preserves input order", {
  fid <- new_folder_id("check_duplicate_files_in_folder order")
  skip_if_null_response(fid, "create_folder for order test")
  on.exit(delete_folder(vol_id = TEST_VOL, folder_id = fid, vb = FALSE), add = TRUE)

  filenames <- c("z.mp4", "a.mp4", "m.mp4")
  result <- check_duplicate_files_in_folder(
    vol_id = TEST_VOL,
    folder_id = fid,
    filenames = filenames,
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_folder(order)")

  expect_equal(result$filename, filenames)
})

test_that("check_duplicate_files_in_folder works with a single filename", {
  fid <- new_folder_id("check_duplicate_files_in_folder single")
  skip_if_null_response(fid, "create_folder for single filename test")
  on.exit(delete_folder(vol_id = TEST_VOL, folder_id = fid, vb = FALSE), add = TRUE)

  result <- check_duplicate_files_in_folder(
    vol_id = TEST_VOL,
    folder_id = fid,
    filenames = "only.mp4",
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_folder(single)")

  expect_equal(nrow(result), 1L)
  expect_equal(result$filename, "only.mp4")
  expect_false(result$exists)
})

test_that("check_duplicate_files_in_folder treats missing folder like empty (all not found)", {
  # Backend accepts the request and queries by folder_id only; staging returns
  # a tibble with exists = FALSE when no files match (same idea as sessions).
  result <- check_duplicate_files_in_folder(
    vol_id = TEST_VOL,
    folder_id = 999999999,
    filenames = c("a.mp4"),
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_folder(non-existent folder)")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$filename, "a.mp4")
  expect_false(result$exists)
})

test_that("check_duplicate_files_in_folder works with verbose mode", {
  fid <- new_folder_id("check_duplicate_files_in_folder vb")
  skip_if_null_response(fid, "create_folder for check_duplicate_files vb")
  on.exit(delete_folder(vol_id = TEST_VOL, folder_id = fid, vb = FALSE), add = TRUE)

  result <- check_duplicate_files_in_folder(
    vol_id = TEST_VOL,
    folder_id = fid,
    filenames = c("vb.mp4"),
    vb = TRUE
  )
  skip_if_null_response(result, "check_duplicate_files_in_folder vb")
  expect_s3_class(result, "tbl_df")
})

test_that("check_duplicate_files_in_folder works with custom request object", {
  fid <- new_folder_id("check_duplicate_files_in_folder custom rq")
  skip_if_null_response(fid, "create_folder for check_duplicate_files custom rq")
  on.exit(delete_folder(vol_id = TEST_VOL, folder_id = fid, vb = FALSE), add = TRUE)

  custom_rq <- databraryr::make_default_request()
  result <- check_duplicate_files_in_folder(
    vol_id = TEST_VOL,
    folder_id = fid,
    filenames = c("custom.mp4"),
    rq = custom_rq,
    vb = FALSE
  )
  skip_if_null_response(result, "check_duplicate_files_in_folder custom rq")
  expect_s3_class(result, "tbl_df")
})

test_that("check_duplicate_files_in_folder rejects invalid filenames", {
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = character(0)))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = NULL))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = 123))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = list("a")))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = TRUE))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = c("a", NA)))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = c("a", "")))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = c("a", "   ")))
})

test_that("check_duplicate_files_in_folder rejects invalid vol_id", {
  expect_error(check_duplicate_files_in_folder(vol_id = -1, folder_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 0, folder_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = "1", folder_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = TRUE, folder_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = c(1, 2), folder_id = 1, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1.5, folder_id = 1, filenames = "a"))
})

test_that("check_duplicate_files_in_folder rejects invalid folder_id", {
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = -1, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 0, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = "1", filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = TRUE, filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = c(1, 2), filenames = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1.5, filenames = "a"))
})

test_that("check_duplicate_files_in_folder rejects invalid vb parameter", {
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = "a", vb = -1))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = "a", vb = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = "a", vb = c(TRUE, FALSE)))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = "a", vb = NULL))
})

test_that("check_duplicate_files_in_folder rejects invalid rq parameter", {
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = "a", rq = "a"))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = "a", rq = -1))
  expect_error(check_duplicate_files_in_folder(vol_id = 1, folder_id = 1, filenames = "a", rq = TRUE))
})
