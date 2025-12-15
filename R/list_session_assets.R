#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Assets in a Databrary Session.
#'
#' #' @description
#' `r lifecycle::badge("experimental")`
#'
#' As of v0.6.3 `list_session_assets()` replaces an older function that is now
#' named `list_volume_session_assets()`.  The older function requires both
#' a volume ID and a session ID. The new function requires only a session ID.
#'
#' @param session_id An integer. A Databrary session number. Default is 9807,
#' the "materials" folder from Databrary volume 1.
#' @param vol_id Optional integer. The volume containing the session. Recent
#' versions of the Databrary API require this value to be supplied because
#' session identifiers are scoped to volumes.
#' @param rq An `httr2` request object. If NULL, a default request is generated
#' from databraryr::make_default_request().
#'
#' @returns A data frame with information about all assets in a volume.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_session_assets() # Session 9807 in volume 1
#' }
#' }
#' @export
list_session_assets <- function(session_id = 9807,
                                vol_id = NULL,
                                vb = options::opt("vb"),
                                rq = NULL) {
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  if (is.null(vol_id)) {
    stop("vol_id must be supplied for list_session_assets(); session identifiers are scoped to volumes.",
         call. = FALSE)
  }
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(is.null(rq) |
                            ("httr2_request" %in% class(rq)))
  
  session <- databraryr::get_session_by_id(
    session_id = session_id,
    vol_id = vol_id,
    vb = vb,
    rq = rq
  )

  if (is.null(session)) {
    return(NULL)
  }

  files <- collect_paginated_get(
    path = sprintf(API_SESSION_FILES, vol_id, session_id),
    rq = rq,
    vb = vb
  )

  if (is.null(files) || length(files) == 0) {
    if (vb)
      message("No assets for session_id ", session_id)
    return(NULL)
  }

  asset_rows <- purrr::map_dfr(files, function(file) {
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
      asset_thumbnail_url = file$thumbnail_url
    )
  })

  asset_rows %>%
    dplyr::mutate(
      session_id = session_id,
      vol_id = vol_id,
      session_name = session$name,
      session_release = session$release_level,
      session_date = session$source_date
    )
}