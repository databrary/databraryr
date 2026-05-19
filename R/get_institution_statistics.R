#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get statistics for an institution
#'
#' Retrieve aggregated statistics for an institution including volume count,
#' file count, and data footprints. Returns NULL (204 No Content) when
#' statistics have not been computed yet.
#'
#' @param institution_id Institution identifier. Must be a positive integer.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#' @inheritParams options_params
#'
#' @return A list with institution statistics including:
#'   - `institution_id`: The institution identifier
#'   - `volumes_number`: Number of volumes
#'   - `files_number`: Number of files
#'   - `uploaded_data_footprint`: Size of uploaded data in bytes
#'   - `transcoded_data_footprint`: Size of transcoded data in bytes
#'   - `soft_deleted_uploaded_data_footprint`: Size of soft-deleted uploaded data
#'   - `soft_deleted_transcoded_data_footprint`: Size of soft-deleted transcoded data
#'   - `created_at`: Timestamp when statistics were created
#'   - `updated_at`: Timestamp when statistics were last updated
#'   Returns NULL when statistics have not been computed or institution is not found.
#' @export
get_institution_statistics <- function(
  institution_id = 1,
  vb = options::opt("vb"),
  rq = NULL
) {
  assertthat::assert_that(
    is.numeric(institution_id),
    length(institution_id) == 1,
    institution_id > 0
  )

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  stats <- perform_api_get(
    path = sprintf(API_INSTITUTION_STATISTICS, institution_id),
    rq = rq,
    vb = vb
  )

  if (is.null(stats)) {
    if (vb) {
      message(
        "Institution statistics for ",
        institution_id,
        " not found or not yet computed."
      )
    }
    return(NULL)
  }

  tibble::tibble(
    institution_id = stats$institution_id,
    volumes_number = stats$volumes_number,
    files_number = stats$files_number,
    uploaded_data_footprint = stats$uploaded_data_footprint,
    transcoded_data_footprint = stats$transcoded_data_footprint,
    soft_deleted_uploaded_data_footprint = stats$soft_deleted_uploaded_data_footprint,
    soft_deleted_transcoded_data_footprint = stats$soft_deleted_transcoded_data_footprint,
    created_at = stats$created_at,
    updated_at = stats$updated_at
  ) |>
    as.list()
}
