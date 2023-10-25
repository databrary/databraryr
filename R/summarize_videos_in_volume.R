#' Summarize Data About Videos in Databrary Volume.
#'
#' @param vol_id An integer. Selected volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return Data frame with the vol_id, number of videos, and total hours.
#' @examples
#' \donttest{
#' \dontrun{
#' summarize_videos_in_volume() # Summary of volume 2
#' }
#' }
#' @export
summarize_videos_in_volume <- function(vol_id = 2, vb = FALSE) {

  # Check parameters
  assertthat::is.number(vol_id)
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(vol_id > 0)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  if (vb)
    message(paste0("Retrieving data about video assets in volume ", vol_id, "."))
  vids <- list_assets_by_type(vol_id, type = 'video')
  
  if (is.null(vids)) {
    if (vb)
      message(paste0('No videos found in volume ', vol_id))
    return (NULL)
  } else {
    if (vb)
      message(paste0('Sumarizing videos in volume ', vol_id))
    vids_df <- dplyr::select(vids, asset_id, duration)
    total_vid_s <- sum(vids$duration) / 1000 # Original units in ms
    total_vid_m <- total_vid_s / 60
    total_vid_hrs <- total_vid_m / 60
    n_vids <- dim(vids_df)[1]
    data.frame(vol_id, n_vids, total_vid_hrs)
  }
}
