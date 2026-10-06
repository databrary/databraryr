#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Sessions in Databrary Volume.
#'
#' @param vol_id Target volume number. Must be a positive integer. Default is 1.
#' @param include_vol_data A Boolean value. Include volume-level metadata
#' or not. Default is FALSE.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. If NULL (the default)
#' a request will be generated, but this will only permit public information
#' to be returned.
#'
#' @returns A data frame with information about all assets in a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_sessions() # Sessions in Volume 1
#' }
#' }
#' @export
list_volume_sessions <-
  function(vol_id = 1,
           include_vol_data = FALSE,
           vb = options::opt("vb"),
           rq = NULL) {
    # Check parameters
    assertthat::assert_that(length(vol_id) == 1)
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id >= 1)

    assertthat::assert_that(is.logical(include_vol_data))
    assertthat::assert_that(length(include_vol_data) == 1)

    validate_flag(vb, "vb")

    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))


    sessions <- collect_paginated_get(
      path = sprintf(API_VOLUME_SESSIONS, vol_id),
      rq = rq,
      vb = vb
    )

    if (is.null(sessions) || length(sessions) == 0) {
      if (vb)
        message("No session data for volume ", vol_id)
      return(NULL)
    }
    if (vb) message("Found n = ",
                    length(sessions),
                    " sessions in vol_id ",
                    vol_id)

    df <- purrr::map_dfr(sessions, function(session) {
      fc <- session[["file_counts"]]

      tibble::tibble(
        session_id = session$id,
        session_name = session$name,
        session_release = session$release_level,
        session_source_date = session$source_date,
        session_native_accessible = file_count_value(fc, "native_accessible"),
        session_native_inaccessible = file_count_value(fc, "native_inaccessible"),
        session_linked_accessible = file_count_value(fc, "linked_accessible"),
        session_linked_inaccessible = file_count_value(fc, "linked_inaccessible"),
        session_has_full_access = session$has_full_access
      )
    })

    if (include_vol_data) {
      volume <- perform_api_get(
        path = sprintf(API_VOLUME_DETAIL, vol_id),
        rq = rq,
        vb = vb
      )

      df <- df |>
        dplyr::mutate(
          vol_id = volume$id,
          vol_name = volume$title,
          vol_created_at = volume$created_at,
          vol_updated_at = volume$updated_at,
          vol_sharing_level = volume$sharing_level,
          vol_access_level = volume$access_level
        )
    }
    tibble::as_tibble(df)
  }
