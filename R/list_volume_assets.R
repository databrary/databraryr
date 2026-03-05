#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Assets in Databrary Volume.
#'
#' @param vol_id Target volume number. Must be a positive integer. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is NULL.
#'
#' @returns A data frame with information about all assets in a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_assets() # Assets in volume 1
#' }
#' }
#' @export
list_volume_assets <- function(vol_id = 1,
                               vb = options::opt("vb"),
                               rq = NULL) {
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)

  validate_flag(vb, "vb")

  sessions <- collect_paginated_get(
    path = sprintf(API_VOLUME_SESSIONS, vol_id),
    rq = rq,
    vb = vb
  )

  if (is.null(sessions) || length(sessions) == 0) {
    if (vb)
      message("No sessions found for volume ", vol_id)
    return(NULL)
  }

  files <- purrr::map(sessions, function(session) {
    session_files <- collect_paginated_get(
      path = sprintf(API_SESSION_FILES, vol_id, session$id),
      rq = rq,
      vb = vb
    )

    if (is.null(session_files) || length(session_files) == 0) {
      return(NULL)
    }

    purrr::map(session_files, function(file) {
      format <- file$format
      uploader <- file$uploader
      tibble::tibble(
        asset_id = file$id,
        asset_name = file$name,
        asset_permission = file$release_level,
        asset_size = file$size,
        asset_mime_type = format$mimetype,
        asset_format_id = format$id,
        asset_format_name = format$name,
        asset_duration = file$duration,
        asset_created_at = file$created_at,
        asset_updated_at = file$updated_at,
        asset_uploader_id = uploader$id,
        asset_uploader_first_name = uploader$first_name,
        asset_uploader_last_name = uploader$last_name,
        asset_sha1 = file$sha1,
        asset_thumbnail_url = file$thumbnail_url,
        session_id = session$id,
        session_name = session$name,
        session_date = session$source_date,
        session_release = session$release_level
      )
    }, .progress = TRUE) %>%
      purrr::list_rbind()
  }) %>%
    purrr::list_rbind()

  if (is.null(files) || nrow(files) == 0) {
    if (vb)
      message("No assets in volume_id ", vol_id, ".")
    return(NULL)
  }

  files
}
