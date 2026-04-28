# snake_case_list() -----------------------------------------------------------
# These are unit tests that exercise the iterative implementation directly
# (no network calls required).

test_that("snake_case_list converts camelCase keys", {
  input <- list(firstName = "Jane", lastName = "Doe")
  result <- databraryr:::snake_case_list(input)
  expect_equal(names(result), c("first_name", "last_name"))
  expect_equal(result$first_name, "Jane")
})

test_that("snake_case_list converts nested structures", {
  input <- list(
    categoryId = 1,
    measures = list(heightCm = 120, weightKg = 25),
    participant = list(
      birthDate = list(year = 2020, monthOfYear = 1, dayOfMonth = 15)
    )
  )
  result <- databraryr:::snake_case_list(input)
  expect_equal(names(result), c("category_id", "measures", "participant"))
  expect_equal(names(result$measures), c("height_cm", "weight_kg"))
  expect_equal(names(result$participant), c("birth_date"))
  expect_equal(names(result$participant$birth_date), c("year", "month_of_year", "day_of_month"))
})

test_that("snake_case_list handles unnamed lists (arrays)", {
  input <- list(
    list(recordId = 1, categoryId = 10),
    list(recordId = 2, categoryId = 20)
  )
  result <- databraryr:::snake_case_list(input)
  expect_null(names(result))
  expect_equal(names(result[[1]]), c("record_id", "category_id"))
  expect_equal(names(result[[2]]), c("record_id", "category_id"))
})

test_that("snake_case_list handles scalar passthrough", {
  expect_equal(databraryr:::snake_case_list(42), 42)
  expect_equal(databraryr:::snake_case_list("hello"), "hello")
  expect_equal(databraryr:::snake_case_list(TRUE), TRUE)
  expect_null(databraryr:::snake_case_list(NULL))
})

test_that("snake_case_list handles empty list", {
  result <- databraryr:::snake_case_list(list())
  expect_equal(result, list())
})

test_that("snake_case_list handles named vectors", {
  input <- c(totalDays = 100, formattedValue = "3 months")
  result <- databraryr:::snake_case_list(input)
  expect_equal(names(result), c("total_days", "formatted_value"))
})

test_that("snake_case_list handles deeply nested record with age (QA repro)", {
  # Simulates the API response that caused stack overflow
  input <- list(
    id = 123,
    volume = 2136,
    categoryId = 1,
    measures = list("1" = "P001", "2" = "Female"),
    birthday = list(metricId = 4, value = list(year = 2020, month = 3, day = 15)),
    age = list(
      years = 5,
      months = 64,
      days = 1948,
      totalDays = 1948,
      formattedValue = "5 years, 4 months",
      isPartial = FALSE,
      isBlurred = FALSE
    )
  )
  result <- databraryr:::snake_case_list(input)

  expect_equal(result$category_id, 1)
  expect_equal(names(result$age), c(
    "years", "months", "days", "total_days",
    "formatted_value", "is_partial", "is_blurred"
  ))
  expect_equal(result$age$total_days, 1948)
  expect_equal(names(result$birthday), c("metric_id", "value"))
})

test_that("snake_case_list handles wide list of records without stack overflow", {
  # Create a wide list simulating a paginated response with many records
  records <- lapply(seq_len(200), function(i) {
    list(
      recordId = i,
      categoryId = 1,
      measures = list(nameField = paste0("Record_", i)),
      age = list(totalDays = i * 10, formattedValue = paste0(i, " days"))
    )
  })
  page <- list(count = 200L, nextUrl = NULL, previousUrl = NULL, results = records)

  result <- databraryr:::snake_case_list(page)
  expect_equal(result$count, 200L)
  expect_equal(names(result$results[[1]]), c("record_id", "category_id", "measures", "age"))
  expect_equal(result$results[[200]]$age$total_days, 2000)
})
