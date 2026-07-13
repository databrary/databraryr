#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get institution metadata
#'
#' @param institution_id Institution identifier. Must be a positive integer.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#' @inheritParams options_params
#'
#' @return List of institution metadata or NULL when inaccessible.
#' @export
get_institution_by_id <- function(institution_id = 12,
                                  vb = options::opt("vb"),
                                  rq = NULL) {
  assertthat::assert_that(is.numeric(institution_id), length(institution_id) == 1, institution_id > 0)

  validate_flag(vb, "vb")

  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  institution <- perform_api_get(
    path = sprintf(API_INSTITUTIONS, institution_id),
    rq = rq,
    vb = vb
  )

  if (is.null(institution)) {
    if (vb) message("Institution ", institution_id, " not found or inaccessible.")
    return(NULL)
  }

  tibble::tibble(
    id = institution$id,
    name = institution$name,
    url = institution$url,
    date_signed = institution$date_signed,
    source = institution$source,
    created_at = institution$created_at,
    updated_at = institution$updated_at,
    has_avatar = institution$has_avatar,
    has_administrators = institution$has_administrators,
    latitude = institution$latitude,
    longitude = institution$longitude,
    manual_coordinates = institution$manual_coordinates
  ) %>%
    as.list()
}
