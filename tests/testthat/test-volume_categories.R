# get_volume_enabled_categories() – validation --------------------------------

test_that("get_volume_enabled_categories rejects invalid vol_id", {
  expect_error(get_volume_enabled_categories(vol_id = -1))
  expect_error(get_volume_enabled_categories(vol_id = 0))
  expect_error(get_volume_enabled_categories(vol_id = "a"))
  expect_error(get_volume_enabled_categories(vol_id = TRUE))
  expect_error(get_volume_enabled_categories(vol_id = c(1, 2)))
  expect_error(get_volume_enabled_categories(vol_id = 1.5))
})

test_that("get_volume_enabled_categories rejects invalid vb", {
  expect_error(get_volume_enabled_categories(vol_id = 1, vb = -1))
  expect_error(get_volume_enabled_categories(vol_id = 1, vb = "a"))
  expect_error(get_volume_enabled_categories(vol_id = 1, vb = c(TRUE, FALSE)))
})

test_that("get_volume_enabled_categories rejects invalid rq", {
  expect_error(get_volume_enabled_categories(vol_id = 1, rq = "a"))
  expect_error(get_volume_enabled_categories(vol_id = 1, rq = -1))
  expect_error(get_volume_enabled_categories(vol_id = 1, rq = TRUE))
})

# set_volume_enabled_categories() – validation --------------------------------

test_that("set_volume_enabled_categories rejects invalid vol_id", {
  expect_error(set_volume_enabled_categories(vol_id = -1, category_ids = c(1)))
  expect_error(set_volume_enabled_categories(vol_id = 0, category_ids = c(1)))
  expect_error(set_volume_enabled_categories(vol_id = "a", category_ids = c(1)))
  expect_error(set_volume_enabled_categories(vol_id = TRUE, category_ids = c(1)))
})

test_that("set_volume_enabled_categories rejects invalid vb", {
  expect_error(set_volume_enabled_categories(vol_id = 1, category_ids = c(1), vb = -1))
  expect_error(set_volume_enabled_categories(vol_id = 1, category_ids = c(1), vb = "a"))
})

test_that("set_volume_enabled_categories rejects invalid rq", {
  expect_error(set_volume_enabled_categories(vol_id = 1, category_ids = c(1), rq = "a"))
  expect_error(set_volume_enabled_categories(vol_id = 1, category_ids = c(1), rq = -1))
})

# enable_volume_category() – validation ---------------------------------------

test_that("enable_volume_category rejects invalid vol_id", {
  expect_error(enable_volume_category(vol_id = -1, category_id = 1))
  expect_error(enable_volume_category(vol_id = 0, category_id = 1))
  expect_error(enable_volume_category(vol_id = "a", category_id = 1))
  expect_error(enable_volume_category(vol_id = TRUE, category_id = 1))
})

test_that("enable_volume_category rejects invalid category_id", {
  expect_error(enable_volume_category(vol_id = 1, category_id = -1))
  expect_error(enable_volume_category(vol_id = 1, category_id = 0))
  expect_error(enable_volume_category(vol_id = 1, category_id = "a"))
  expect_error(enable_volume_category(vol_id = 1, category_id = TRUE))
  expect_error(enable_volume_category(vol_id = 1, category_id = c(1, 2)))
})

test_that("enable_volume_category rejects invalid vb", {
  expect_error(enable_volume_category(vol_id = 1, category_id = 1, vb = -1))
  expect_error(enable_volume_category(vol_id = 1, category_id = 1, vb = "a"))
})

# disable_volume_category() – validation --------------------------------------

test_that("disable_volume_category rejects invalid vol_id", {
  expect_error(disable_volume_category(vol_id = -1, category_id = 1))
  expect_error(disable_volume_category(vol_id = 0, category_id = 1))
  expect_error(disable_volume_category(vol_id = "a", category_id = 1))
  expect_error(disable_volume_category(vol_id = TRUE, category_id = 1))
})

test_that("disable_volume_category rejects invalid category_id", {
  expect_error(disable_volume_category(vol_id = 1, category_id = -1))
  expect_error(disable_volume_category(vol_id = 1, category_id = 0))
  expect_error(disable_volume_category(vol_id = 1, category_id = "a"))
  expect_error(disable_volume_category(vol_id = 1, category_id = TRUE))
  expect_error(disable_volume_category(vol_id = 1, category_id = c(1, 2)))
})

test_that("disable_volume_category rejects invalid vb", {
  expect_error(disable_volume_category(vol_id = 1, category_id = 1, vb = -1))
  expect_error(disable_volume_category(vol_id = 1, category_id = 1, vb = "a"))
})

# Integration tests (require staging credentials) ----------------------------

login_test_account()

test_that("get_volume_enabled_categories returns a list", {
  result <- get_volume_enabled_categories(vol_id = 1, vb = FALSE)
  skip_if_null_response(result, "get_volume_enabled_categories")

  expect_type(result, "list")
})

test_that("enable and disable category round-trip works", {
  vol_id <- 1777

  initial <- get_volume_enabled_categories(vol_id = vol_id, vb = FALSE)
  skip_if_null_response(initial, "get_volume_enabled_categories for round-trip")

  initial_ids <- vapply(initial, function(c) as.integer(c$id), integer(1))
  test_cat <- 6L

  if (test_cat %in% initial_ids) {
    disable_volume_category(vol_id = vol_id, category_id = test_cat, vb = FALSE)
  }

  enable_result <- enable_volume_category(
    vol_id = vol_id, category_id = test_cat, vb = FALSE
  )
  skip_if_null_response(enable_result, "enable_volume_category")

  after_enable <- get_volume_enabled_categories(vol_id = vol_id, vb = FALSE)
  enabled_ids <- vapply(after_enable, function(c) as.integer(c$id), integer(1))
  expect_true(test_cat %in% enabled_ids)

  disable_result <- disable_volume_category(
    vol_id = vol_id, category_id = test_cat, vb = FALSE
  )
  skip_if_null_response(disable_result, "disable_volume_category")

  after_disable <- get_volume_enabled_categories(vol_id = vol_id, vb = FALSE)
  disabled_ids <- vapply(after_disable, function(c) as.integer(c$id), integer(1))
  expect_false(test_cat %in% disabled_ids)

  set_volume_enabled_categories(
    vol_id = vol_id, category_ids = initial_ids, vb = FALSE
  )
})
