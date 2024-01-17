#' List Assets in Databrary Volume.
#'
#' @param vol_id Target volume number.
#' @param vb A boolean value.
#' @param rq An `httr2` request object.
#' @returns A data frame with information about all assets in a volume.
#' @examples
#' \donttest{
#' \dontrun{
#' list_assets_in_volume() # Assets in volume 1
#' }
#' }
#' @export
list_volume_assets <- function(vol_id = 1, vb = FALSE, rq = NULL) {
  
  # Check parameters
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  vol_list <- get_volume_by_id(vol_id, rq)
  if (!("containers" %in% names(vol_list))) {
    if (vb) message("No session/containers data from volume ", vol_id)
    return(NULL)
  }
  
  purrr::map(vol_list$containers, get_assets_from_session, ignore_materials = FALSE, .progress = TRUE) |>
    purrr::list_rbind() |>
    dplyr::mutate(vol_id = vol_id,
                  vol_name = vol_list$name,
                  vol_creation = vol_list$creation,
                  vol_permission = vol_list$permission,
                  vol_publicaccess = vol_list$publicaccess)
}

#-------------------------------------------------------------------------------
#' Helper function for list_volume_assets
get_assets_from_session <- function(volume_container, ignore_materials = TRUE) {
  
  # ignore materials
  if (ignore_materials) {
    if ("top" %in% names(volume_container)) return(NULL)    
  }

  assets_df <- purrr::map(volume_container$assets, as.data.frame) |>
    purrr::list_rbind()
  
  # ignore empty sessions
  if (dim(assets_df)[1] == 0) return(NULL)
  
  if (!('size' %in% names(assets_df))) assets_df$size = NA
  if (!('duration' %in% names(assets_df))) assets_df$duration = NA
  if (!('name' %in% names(assets_df))) assets_df$name = NA

  assets_df |>
    dplyr::select(id, format, duration, name, permission, size) |>
    dplyr::rename(asset_id = id,
                  asset_format = format,
                  asset_name = name,
                  asset_duration = duration,
                  asset_permision = permission,
                  asset_size = size) |>
    dplyr::mutate(session_id = volume_container$id,
                  session_date = volume_container$date,
                  session_release = volume_container$release)
}

