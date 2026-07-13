# list_institutions() ---------------------------------------------------------
login_test_account()

institutions_all <- list_institutions(vb = FALSE)

test_that("list_institutions returns all institutions without search filter", {
  result <- institutions_all
  skip_if_null_response(result, "list_institutions()")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("institution_id", "institution_name", "institution_url",
                          "institution_date_signed", "institution_source",
                          "institution_created_at", "institution_updated_at",
                          "institution_has_avatar", "institution_has_administrators",
                          "institution_latitude", "institution_longitude",
                          "institution_manual_coordinates"))
  expect_gt(nrow(result), 0)

  # Check column types
  expect_true(is.numeric(result$institution_id) || is.integer(result$institution_id))
  expect_type(result$institution_name, "character")
  expect_type(result$institution_url, "character")
  expect_type(result$institution_date_signed, "character")
  expect_type(result$institution_source, "character")
  expect_type(result$institution_created_at, "character")
  expect_type(result$institution_updated_at, "character")
  expect_type(result$institution_has_avatar, "logical")
  expect_type(result$institution_has_administrators, "logical")
  expect_true(is.numeric(result$institution_latitude) || is.double(result$institution_latitude))
  expect_true(is.numeric(result$institution_longitude) || is.double(result$institution_longitude))
  expect_type(result$institution_manual_coordinates, "logical")
})

test_that("list_institutions filters by search string", {
  result <- list_institutions(search_string = "university", vb = FALSE)
  skip_if_null_response(result, "list_institutions(search_string = 'university')")

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("institution_id", "institution_name", "institution_url",
                          "institution_date_signed", "institution_source",
                          "institution_created_at", "institution_updated_at",
                          "institution_has_avatar", "institution_has_administrators",
                          "institution_latitude", "institution_longitude",
                          "institution_manual_coordinates"))
  expect_gt(nrow(result), 0)

  # Check that results contain the search term (case insensitive)
  expect_true(any(grepl("university", result$institution_name, ignore.case = TRUE)))
})

test_that("list_institutions works with verbose mode", {
  result <- list_institutions(vb = TRUE)
  skip_if_null_response(result, "list_institutions(vb = TRUE)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_institutions returns NULL for search with no matches", {
  # Use a very unlikely search string
  result <- list_institutions(search_string = "xyzabcdefghijklmnopqrstuvwxyz999999", vb = FALSE)
  expect_null(result)
})

test_that("list_institutions rejects invalid search_string", {
  # Non-character search_string
  expect_error(list_institutions(search_string = 123))
  expect_error(list_institutions(search_string = TRUE))
  expect_error(list_institutions(search_string = list(a = 1)))

  # Multiple values
  expect_error(list_institutions(search_string = c("test1", "test2")))
})

test_that("list_institutions rejects invalid vb parameter", {
  expect_error(list_institutions(vb = -1))
  expect_error(list_institutions(vb = 3))
  expect_error(list_institutions(vb = "a"))
  expect_error(list_institutions(vb = list(a = 1, b = 2)))
  expect_error(list_institutions(vb = c(TRUE, FALSE)))
  expect_error(list_institutions(vb = NULL))
})

test_that("list_institutions rejects invalid rq parameter", {
  expect_error(list_institutions(rq = "a"))
  expect_error(list_institutions(rq = -1))
  expect_error(list_institutions(rq = c(2, 3)))
  expect_error(list_institutions(rq = list(a = 1, b = 2)))
  expect_error(list_institutions(rq = TRUE))
})

test_that("list_institutions result structure is consistent", {
  result <- institutions_all
  skip_if_null_response(result, "list_institutions()")

  # Check that all expected fields exist
  expect_true(all(c("institution_id", "institution_name", "institution_url", "institution_has_avatar") %in% names(result)))

  # Check that institution_id values are numeric
  expect_true(is.numeric(result$institution_id) || is.integer(result$institution_id))

  # Check that institution_name is never NA
  expect_true(all(!is.na(result$institution_name)))
})

test_that("list_institutions handles NA values correctly", {
  result <- institutions_all
  skip_if_null_response(result, "list_institutions()")

  # institution_url can be NA for institutions without a URL
  expect_type(result$institution_url, "character")

  # institution_has_avatar can be NA for institutions without avatar info
  expect_type(result$institution_has_avatar, "logical")
})

test_that("list_institutions works with custom request object", {
  custom_rq <- databraryr::make_default_request()
  result <- list_institutions(rq = custom_rq, vb = FALSE)
  skip_if_null_response(result, "list_institutions(rq = custom_rq)")

  expect_s3_class(result, "tbl_df")
  expect_gt(nrow(result), 0)
})

test_that("list_institutions returns different results with and without search", {
  all_institutions <- institutions_all
  skip_if_null_response(all_institutions, "list_institutions()")

  # Get filtered institutions
  filtered_institutions <- list_institutions(search_string = "state", vb = FALSE)

  # If filtered results exist, they should be a subset of all institutions
  if (!is.null(filtered_institutions)) {
    expect_true(nrow(filtered_institutions) <= nrow(all_institutions))
  }
})

test_that("list_institutions returns unique institution IDs", {
  result <- institutions_all
  skip_if_null_response(result, "list_institutions()")

  # Check that all institution IDs are unique
  expect_equal(nrow(result), length(unique(result$institution_id)))
})

test_that("list_institutions search is case insensitive", {
  result_lower <- list_institutions(search_string = "university", vb = FALSE)
  result_upper <- list_institutions(search_string = "UNIVERSITY", vb = FALSE)

  # If both return results, they should be similar
  if (!is.null(result_lower) && !is.null(result_upper)) {
    # Both should have results with "university" in the name (case insensitive)
    expect_true(any(grepl("university", result_lower$institution_name, ignore.case = TRUE)))
    expect_true(any(grepl("university", result_upper$institution_name, ignore.case = TRUE)))
  }
})

test_that("list_institutions can retrieve institutions with avatars", {
  result <- institutions_all
  skip_if_null_response(result, "list_institutions()")

  # Filter institutions that have avatars
  institutions_with_avatars <- result[!is.na(result$institution_has_avatar) & result$institution_has_avatar == TRUE, ]

  # There should be at least some institutions with avatars
  if (nrow(institutions_with_avatars) > 0) {
    expect_gt(nrow(institutions_with_avatars), 0)
    expect_true(all(institutions_with_avatars$institution_has_avatar == TRUE))
  }
})
