#' @eval options::as_params()
#' @name options_params
#'
NULL

#' @noRd
empty_volume_records_tibble <- function() {
  tibble::tibble(
    record_id = integer(0),
    record_volume = integer(0),
    record_category_id = integer(0),
    record_measures = list(),
    record_birthday = character(0),
    age_years = integer(0),
    age_months = integer(0),
    age_days = integer(0),
    age_total_days = integer(0),
    age_formatted = character(0),
    age_is_estimated = logical(0),
    age_is_blurred = logical(0)
  )
}

#' List Records in Databrary Volume
#'
#' @description Retrieve all records (participant data with measures) from a
#' specific Databrary volume. Records contain participant information including
#' age, birthday, category, and associated measures collected during sessions.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param category_id Optional numeric category identifier to filter records
#'   by category type.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @return A tibble containing metadata for each record including id, volume,
#'   category_id, measures, birthday, and age information. Returns an empty
#'   tibble (with the same columns) when the volume has no records, or `NULL`
#'   when the API call fails (e.g. non-existent volume).
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # List all records in volume 1
#' list_volume_records(vol_id = 1)
#'
#' # Filter records by category
#' list_volume_records(vol_id = 1, category_id = 2)
#'
#' # With verbose output
#' list_volume_records(vol_id = 1, vb = TRUE)
#' }
#' }
#' @export
list_volume_records <- function(vol_id = 1,
                                category_id = NULL,
                                vb = options::opt("vb"),
                                rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  assertthat::assert_that(vol_id == floor(vol_id), msg = "vol_id must be an integer")

  if (!is.null(category_id)) {
    assertthat::assert_that(length(category_id) == 1)
    assertthat::assert_that(is.numeric(category_id))
    assertthat::assert_that(category_id > 0)
    assertthat::assert_that(category_id == floor(category_id), msg = "category_id must be an integer")
  }

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))

  # Build params list
  params <- list()
  if (!is.null(category_id)) {
    params$category_id <- category_id
  }

  # Perform API call
  records <- collect_paginated_get(
    path = sprintf(API_VOLUME_RECORDS, vol_id),
    params = params,
    rq = rq,
    vb = vb
  )

  if (is.null(records)) {
    return(NULL)
  }

  if (length(records) == 0) {
    if (vb) {
      message("No records found with category_id = ",
              category_id,
              " for volume ",
              vol_id)
    }
    return(empty_volume_records_tibble())
  }

  if (vb)
    message(
      "Found n = ",
      length(records),
      " records with category_id = ",
      category_id,
      " in volume ",
      vol_id
    )

  # Process records into tibble
  purrr::map_dfr(records, function(record) {
    # Process age if present
    age_years <- NA_integer_
    age_months <- NA_integer_
    age_days <- NA_integer_
    age_total_days <- NA_integer_
    age_formatted <- NA_character_
    age_is_estimated <- NA
    age_is_blurred <- NA

    if (!is.null(record$age)) {
      age_years <- if (!is.null(record$age$years)) {
        record$age$years
      } else {
        NA_integer_
      }
      age_months <- if (!is.null(record$age$months)) {
        record$age$months
      } else {
        NA_integer_
      }
      age_days <- if (!is.null(record$age$days)) {
        record$age$days
      } else {
        NA_integer_
      }
      age_total_days <- if (!is.null(record$age$total_days)) {
        record$age$total_days
      } else {
        NA_integer_
      }
      age_formatted <- if (!is.null(record$age$formatted_value)) {
        record$age$formatted_value
      } else {
        NA_character_
      }
      age_is_estimated <- if (!is.null(record$age$is_estimated)) {
        record$age$is_estimated
      } else {
        NA
      }
      age_is_blurred <- if (!is.null(record$age$is_blurred)) {
        record$age$is_blurred
      } else {
        NA
      }
    }

    tibble::tibble(
      record_id = record$id,
      record_volume = record$volume,
      record_category_id = record$category_id,
      record_measures = list(record$measures),
      record_birthday = if (is.null(record$birthday)) {
        NA_character_
      } else {
        as.character(record$birthday)
      },
      age_years = age_years,
      age_months = age_months,
      age_days = age_days,
      age_total_days = age_total_days,
      age_formatted = age_formatted,
      age_is_estimated = age_is_estimated,
      age_is_blurred = age_is_blurred
    )
  })
}
