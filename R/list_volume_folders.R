#' @eval options::as_params()
#' @name options_params
#'
NULL

#' List Folders in a Databrary Volume.
#'
#' @param vol_id Target volume number. Must be a positive integer. Default is 1.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Defaults to `NULL`.
#'
#' @returns A tibble with metadata about folders in the selected volume, or
#'   `NULL` when no folders are available.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' list_volume_folders() # Folders in volume 1
#' }
#' }
#' @export
list_volume_folders <- function(vol_id = 1,
                                vb = options::opt("vb"),
                                rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) ||
                            inherits(rq, "httr2_request"))
  
  folders <- collect_paginated_get(
    path = sprintf(API_VOLUME_FOLDERS, vol_id),
    rq = rq,
    vb = vb
  )
  
  if (is.null(folders) || length(folders) == 0) {
    if (vb) {
      message("No folders available for volume ", vol_id)
    }
    return(NULL)
  }
  
  purrr::map_dfr(folders, function(folder) {
    volume_value <- folder$volume
    if (is.null(volume_value)) {
      volume_value <- vol_id
    }
    
    tibble::tibble(
      folder_id = folder$id,
      folder_name = folder$name,
      folder_release = folder$release_level,
      folder_file_count = folder$file_count,
      folder_accessible_file_count = folder$accessible_file_count,
      folder_has_full_access = folder$has_full_access,
      folder_contains_different_release_levels = folder$contains_different_release_levels,
      folder_created_at = folder$created_at,
      folder_updated_at = folder$updated_at,
      folder_source_date = folder$source_date,
      vol_id = volume_value
    )
  })
}
