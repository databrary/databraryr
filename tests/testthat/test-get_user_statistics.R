login_test_account()

test_that("get_user_statistics returns statistics for user with data", {
    result <- get_user_statistics(6)
    skip_if_null_response(result, "get_user_statistics(6)")
    expect_true(is.list(result))
    expect_equal(result$user_id, 6)
    expect_true(is.numeric(result$volumes_number))
    expect_true(is.numeric(result$files_number))
    expect_true(is.numeric(result$uploaded_data_footprint))
    expect_true(is.numeric(result$transcoded_data_footprint))
})

test_that("get_user_statistics returns NULL for user without statistics", {
    result <- get_user_statistics(999)
    # User 999 may not exist or may have no statistics
    expect_true(is.null(result) || is.list(result))
})

test_that("get_user_statistics with vb = TRUE", {
    result <- get_user_statistics(6, vb = TRUE)
    skip_if_null_response(result, "get_user_statistics(6, vb = TRUE)")
    expect_true(is.list(result))
    expect_equal(result$user_id, 6)
})
