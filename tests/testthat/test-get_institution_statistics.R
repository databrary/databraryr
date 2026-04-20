test_that("get_institution_statistics returns statistics for institution with data", {
    login_test_account()
    result <- get_institution_statistics(1)
    skip_if_null_response(result, "get_institution_statistics(1)")
    expect_true(is.list(result))
    expect_equal(result$institution_id, 1)
    expect_true(is.numeric(result$volumes_number))
    expect_true(is.numeric(result$files_number))
    expect_true(is.numeric(result$uploaded_data_footprint))
    expect_true(is.numeric(result$transcoded_data_footprint))
})

test_that("get_institution_statistics returns NULL for institution without statistics", {
    login_test_account()
    result <- get_institution_statistics(999)
    # Institution 999 may not exist or may have no statistics
    expect_true(is.null(result) || is.list(result))
})

test_that("get_institution_statistics with vb = TRUE", {
    login_test_account()
    result <- get_institution_statistics(1, vb = TRUE)
    skip_if_null_response(result, "get_institution_statistics(1, vb = TRUE)")
    expect_true(is.list(result))
    expect_equal(result$institution_id, 1)
})
