#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Basic Volume Info.
#'
#' @param vol_id Target volume number. Must be a positive integer. Defaults to 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. If NULL (the default).
#' a request will be generated, but this will only permit public information
#' to be returned.
#'
#' @returns A data frame with basic information about a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_info() # Sessions in Volume 1
#' }
#' }
#'
#' @export
list_volume_info <-
  function(vol_id = 1,
           vb = options::opt("vb"),
           rq = NULL) {
    # Check parameters
    assertthat::assert_that(length(vol_id) == 1)
    assertthat::assert_that(is.numeric(vol_id))
    assertthat::assert_that(vol_id >= 1)

    validate_flag(vb, "vb")

    assertthat::assert_that(is.null(rq) |
                              ("httr2_request" %in% class(rq)))

    volume <- databraryr::get_volume_by_id(vol_id = vol_id, vb = vb, rq = rq)
    if (is.null(volume)) {
      return(NULL)
    } else {
      if (vb) message("Summarising volume detail...")

      owner_connection <- volume$owner_connection
      owner_institution <- volume$owner_institution

      session_count <- volume$session_count[[1]]
      session_count_shared <- volume$session_count_shared[[1]]

      file_counts <- volume$file_counts[[1]]

      fundings <- perform_api_get(
        path = sprintf(API_VOLUME_FUNDINGS, vol_id),
        rq = rq,
        vb = vb
      )
      n_vol_funders <- if (is.null(fundings)) 0 else length(fundings)

      vol_assets <- list_volume_assets(vol_id = vol_id, vb = vb, rq = rq)

      if (is.null(vol_assets) || nrow(vol_assets) == 0) {
        n_vol_assets <- 0
        tot_vol_size_mb <- 0
        tot_vol_dur_hrs <- 0
      } else {
        n_vol_assets <- nrow(vol_assets)
        tot_vol_size_mb <- round(sum(stats::na.omit(vol_assets$asset_size)) / (1024 * 1024), 3)
        tot_vol_dur_hrs <- if ("asset_duration" %in% names(vol_assets)) {
          round(sum(stats::na.omit(vol_assets$asset_duration)) / 3600, 3)
        } else {
          NA_real_
        }
      }

      tibble::tibble(
        vol_id = volume$id,
        vol_name = volume$title,
        vol_short_name = volume$short_name,
        vol_desc = volume$description,
        vol_created_at = volume$created_at,
        vol_updated_at = volume$updated_at,
        vol_sharing_level = volume$sharing_level,
        vol_access_level = volume$access_level,
        vol_owner_connection = owner_connection,
        vol_owner_institution = owner_institution,
        vol_n_sessions = session_count,
        vol_n_sessions_shared = session_count_shared,
        vol_file_counts = list(file_counts),
        vol_n_assets = n_vol_assets,
        vol_tot_size_mb = tot_vol_size_mb,
        vol_tot_dur_hrs = tot_vol_dur_hrs,
        vol_n_funders = n_vol_funders,
        vol_enabled_categories = list(volume$enabled_categories[[1]]),
        vol_enabled_metrics = list(volume$enabled_metrics[[1]]),
        vol_citation = list(volume$citation[[1]])
      )
    }
  }
