# get_volume_collaborator_by_id() --------------------------------------------
login_test_account()

first_valid_collaborator_id <- function(collaborators) {
  ids <- collaborators$collaborator_id
  ok <- !is.na(ids) & ids > 0
  if (!any(ok)) {
    return(NULL)
  }
  ids[which(ok)[1]]
}

test_that("get_volume_collaborator_by_id retrieves valid collaborator", {
  # First get a list of collaborators to find a valid collaborator_id
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", test_collaborator_id))

  expect_type(result, "list")
  expect_named(result, c("collaborator_id", "volume", "user", "sponsor", "sponsorship", "is_publicly_visible", "access_level", "expiration_date", "sponsored_users"))
  expect_equal(result$collaborator_id, test_collaborator_id)
  expect_equal(result$volume, 1)
})

test_that("get_volume_collaborator_by_id returns NULL for non-existent collaborator", {
  # Use a very large ID that likely doesn't exist
  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 999999, vb = FALSE)
  expect_null(result)
})

test_that("get_volume_collaborator_by_id returns NULL for non-existent volume", {
  result <- get_volume_collaborator_by_id(vol_id = 999999, collaborator_id = 1, vb = FALSE)
  expect_null(result)
})

test_that("get_volume_collaborator_by_id works with verbose mode", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id, vb = TRUE)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d, vb = TRUE)", test_collaborator_id))

  expect_type(result, "list")
  expect_true(!is.null(result$collaborator_id))
})

test_that("get_volume_collaborator_by_id rejects invalid vol_id", {
  # Negative ID
  expect_error(get_volume_collaborator_by_id(vol_id = -1, collaborator_id = 1))

  # Zero ID
  expect_error(get_volume_collaborator_by_id(vol_id = 0, collaborator_id = 1))

  # Non-numeric ID
  expect_error(get_volume_collaborator_by_id(vol_id = "1", collaborator_id = 1))
  expect_error(get_volume_collaborator_by_id(vol_id = TRUE, collaborator_id = 1))
  expect_error(get_volume_collaborator_by_id(vol_id = list(a = 1), collaborator_id = 1))

  # Multiple values
  expect_error(get_volume_collaborator_by_id(vol_id = c(1, 2), collaborator_id = 1))

  # Decimal/non-integer
  expect_error(get_volume_collaborator_by_id(vol_id = 1.5, collaborator_id = 1))
  expect_error(get_volume_collaborator_by_id(vol_id = 2.7, collaborator_id = 1))

  # NULL
  expect_error(get_volume_collaborator_by_id(vol_id = NULL, collaborator_id = 1))

  # NA
  expect_error(get_volume_collaborator_by_id(vol_id = NA, collaborator_id = 1))
})

test_that("get_volume_collaborator_by_id rejects invalid collaborator_id", {
  # Negative ID
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = -1))

  # Zero ID
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 0))

  # Non-numeric ID
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = "1"))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = TRUE))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = list(a = 1)))

  # Multiple values
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = c(1, 2)))

  # Decimal/non-integer
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1.5))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 2.7))

  # NULL
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = NULL))

  # NA
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = NA))
})

test_that("get_volume_collaborator_by_id rejects invalid vb parameter", {
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, vb = -1))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, vb = 3))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, vb = "a"))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, vb = list(a = 1, b = 2)))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, vb = c(TRUE, FALSE)))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, vb = NULL))
})

test_that("get_volume_collaborator_by_id rejects invalid rq parameter", {
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, rq = "a"))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, rq = -1))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, rq = c(2, 3)))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, rq = list(a = 1, b = 2)))
  expect_error(get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 1, rq = TRUE))
})

test_that("get_volume_collaborator_by_id result structure is consistent", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", test_collaborator_id))

  # Check that all expected fields exist
  expect_true(all(c("collaborator_id", "volume", "user", "sponsor", "sponsorship", "is_publicly_visible", "access_level", "expiration_date", "sponsored_users") %in% names(result)))

  # Check field types
  expect_true(is.numeric(result$collaborator_id) || is.integer(result$collaborator_id))
  expect_true(is.numeric(result$volume) || is.integer(result$volume))
  expect_true(is.list(result$user) || is.null(result$user))
  expect_true(is.logical(result$is_publicly_visible))
  expect_true(is.character(result$access_level))

  # Check that collaborator_id matches the requested ID
  expect_equal(result$collaborator_id, test_collaborator_id)

  # Check that volume matches requested volume
  expect_equal(result$volume, 1)
})

test_that("get_volume_collaborator_by_id handles user structure correctly", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", test_collaborator_id))

  # If user exists, check its structure
  if (!is.null(result$user)) {
    expect_type(result$user, "list")
    expected_fields <- c("user_id", "first_name", "last_name", "email", "is_authorized_investigator", "has_avatar")
    expect_true(all(expected_fields %in% names(result$user)))
    expect_true(!is.null(result$user$user_id))
  }
})

test_that("get_volume_collaborator_by_id handles sponsor structure correctly", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", test_collaborator_id))

  # If sponsor exists, check its structure
  if (!is.null(result$sponsor)) {
    expect_type(result$sponsor, "list")
    expected_fields <- c("sponsor_id", "first_name", "last_name", "email")
    expect_true(all(expected_fields %in% names(result$sponsor)))
    expect_true(!is.null(result$sponsor$sponsor_id))
  }
})

test_that("get_volume_collaborator_by_id handles access_level correctly", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", test_collaborator_id))

  # access_level should be a character string
  expect_type(result$access_level, "character")
  expect_true(nchar(result$access_level) > 0)
})

test_that("get_volume_collaborator_by_id works with custom request object", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  custom_rq <- databraryr::make_default_request()
  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id, rq = custom_rq)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d, rq = custom_rq)", test_collaborator_id))

  expect_type(result, "list")
  expect_equal(result$collaborator_id, test_collaborator_id)
})

test_that("get_volume_collaborator_by_id can retrieve multiple different collaborators", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  if (nrow(collaborators) >= 2) {
    # Filter out NA values and ensure we have valid IDs
    valid_ids <- collaborators$collaborator_id[!is.na(collaborators$collaborator_id) & collaborators$collaborator_id > 0]

    if (length(valid_ids) >= 2) {
      collaborator_id_1 <- valid_ids[1]
      collaborator_id_2 <- valid_ids[2]

      result1 <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = collaborator_id_1, vb = FALSE)
      result2 <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = collaborator_id_2, vb = FALSE)

      skip_if_null_response(result1, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", collaborator_id_1))
      skip_if_null_response(result2, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", collaborator_id_2))

      # If both exist, they should be different
      expect_false(identical(result1$collaborator_id, result2$collaborator_id))
      expect_equal(result1$collaborator_id, collaborator_id_1)
      expect_equal(result2$collaborator_id, collaborator_id_2)
    }
  }
})

test_that("get_volume_collaborator_by_id returns complete structure with all fields", {
  collaborators <- list_volume_collaborators(vol_id = 1, vb = FALSE)
  skip_if_null_response(collaborators, "list_volume_collaborators(vol_id = 1)")

  test_collaborator_id <- first_valid_collaborator_id(collaborators)
  skip_if(is.null(test_collaborator_id), "no row with collaborator_id > 0 from API")

  result <- get_volume_collaborator_by_id(vol_id = 1, collaborator_id = test_collaborator_id)
  skip_if_null_response(result, sprintf("get_volume_collaborator_by_id(vol_id = 1, collaborator_id = %d)", test_collaborator_id))

  # Collaborator should have all expected fields
  expect_length(result, 9)
  expect_named(result, c("collaborator_id", "volume", "user", "sponsor", "sponsorship", "is_publicly_visible", "access_level", "expiration_date", "sponsored_users"))
})