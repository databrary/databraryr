#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get Summary Data About A Databrary Volume
#'
#' @param vol_id Volume ID. Must be a positive integer. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. If NULL (the default), a new request
#' is generated using `make_default_request()`. To access restricted data,
#' the user must login with a specific request object using `login_db()`.
#'
#' @returns A tibble with summary information about a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \dontrun{
#' get_volume_by_id() # Default is Volume 1
#' }
#'
#' @export
get_volume_by_id <- function(vol_id = 1,
                             vb = options::opt("vb"),
                             rq = NULL) {
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))

  if (vb)
    message("Retrieving data for vol_id ", vol_id, ".")

  volume <- perform_api_get(
    path = sprintf(API_VOLUME_DETAIL, vol_id),
    rq = rq,
    vb = vb
  )

  if (is.null(volume)) {
    message("Cannot access requested resource on Databrary. Exiting.")
    return(NULL)
  }

  tibble::tibble(
    id = volume$id,
    updated_at = volume$updated_at,
    created_at = volume$created_at,
    title = volume$title,
    description = purrr::pluck(volume, "description", .default = NA_character_),
    short_name = purrr::pluck(volume, "short_name", .default = NA_character_),
    owner_connection = list(purrr::pluck(volume, "owner_connection", .default = NULL)),
    owner_institution = list(volume$owner_institution),
    sharing_level = volume$sharing_level,
    access_level = volume$access_level,
    has_admin_access = purrr::pluck(volume, "has_admin_access", .default = NA),
    fundings = list(purrr::pluck(volume, "fundings", .default = NULL)),
    coauthors = list(purrr::pluck(volume, "coauthors", .default = NULL)),
    links = list(purrr::pluck(volume, "links", .default = NULL)),
    enabled_categories = list(purrr::pluck(volume, "enabled_categories", .default = NULL)),
    enabled_metrics = list(purrr::pluck(volume, "enabled_metrics", .default = NULL)),
    citation = list(purrr::pluck(volume, "citation", .default = NULL)),
    session_count = volume$session_count,
    session_count_shared = volume$session_count_shared,
    participant_count = purrr::pluck(volume, "participant_count", .default = NA_integer_),
    participant_gender_counts = list(purrr::pluck(volume, "participant_gender_counts", .default = NULL)),
    file_counts = list(volume$file_counts),
    thumbnail = list(purrr::pluck(volume, "thumbnail", .default = NULL))
  )
}
