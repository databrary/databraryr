#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get statistics for a user
#'
#' Retrieve aggregated statistics for a user including volume count,
#' file count, and data footprints. Returns NULL (204 No Content) when
#' statistics have not been computed yet.
#'
#' @param user_id User identifier. Must be a positive integer.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#' @inheritParams options_params
#'
#' @return A list with user statistics including:
#'   - `user_id`: The user identifier
#'   - `volumes_number`: Number of volumes
#'   - `files_number`: Number of files
#'   - `uploaded_data_footprint`: Size of uploaded data in bytes
#'   - `transcoded_data_footprint`: Size of transcoded data in bytes
#'   - `soft_deleted_uploaded_data_footprint`: Size of soft-deleted uploaded data
#'   - `soft_deleted_transcoded_data_footprint`: Size of soft-deleted transcoded data
#'   - `created_at`: Timestamp when statistics were created
#'   - `updated_at`: Timestamp when statistics were last updated
#'   Returns NULL when statistics have not been computed or user is not found.
#' @export
get_user_statistics <- function(user_id, vb = options::opt("vb"), rq = NULL) {
  assertthat::assert_that(
    is.numeric(user_id),
    length(user_id) == 1,
    user_id > 0
  )

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  stats <- perform_api_get(
    path = sprintf(API_USER_STATISTICS, user_id),
    rq = rq,
    vb = vb
  )

  if (is.null(stats)) {
    if (vb) {
      message(
        "User statistics for ",
        user_id,
        " not found or not yet computed."
      )
    }
    return(NULL)
  }

  tibble::tibble(
    user_id = stats$user_id,
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
