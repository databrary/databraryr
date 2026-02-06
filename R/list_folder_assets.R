#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Assets Within a Databrary Folder.
#'
#' @param folder_id Folder identifier scoped to the given volume. Must be a
#' positive integer. Default is 9807.
#' @param vol_id Volume containing the folder. Required for Django API calls.
#' Must be a positive integer. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @returns A tibble with metadata for files contained in the folder, or
#'   `NULL` when the folder has no accessible assets.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_folder_assets(folder_id = 1, vol_id = 1)
#' }
#' }
#' @export
list_folder_assets <- function(folder_id = 9807,
                               vol_id = 1,
                               vb = options::opt("vb"),
                               rq = NULL) {
  assertthat::assert_that(length(folder_id) == 1)
  assertthat::assert_that(is.numeric(folder_id))
  assertthat::assert_that(folder_id >= 1)
  
  if (is.null(vol_id)) {
    stop(
      "vol_id must be supplied for list_folder_assets(); folder identifiers are scoped to volumes.",
      call. = FALSE
    )
  }
  
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  validate_flag(vb, "vb")
  
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  folder <- databraryr::get_folder_by_id(
    folder_id = folder_id,
    vol_id = vol_id,
    vb = vb,
    rq = rq
  )
  
  if (is.null(folder)) {
    return(NULL)
  }
  
  files <- collect_paginated_get(
    path = sprintf(API_FOLDER_FILES, vol_id, folder_id),
    rq = rq,
    vb = vb
  )
  
  if (is.null(files) || length(files) == 0) {
    if (vb) {
      message("No assets for folder_id ", folder_id)
    }
    return(NULL)
  }
  
  file_rows <- purrr::map_dfr(files, function(file) {
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
      format_extension = format$extension,
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
  
  file_rows %>%
    dplyr::mutate(
      folder_id = folder_id,
      vol_id = vol_id,
      folder_name = folder$name,
      folder_release = folder$release_level,
      folder_source_date = folder$source_date
    )
}
