#' Summarize the number and hours of videos in a volume.
#'
#' @param vol_id Selected volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return Data frame with the vol_id, number of videos, and total hours.
#' @examples
#' summarize_videos_in_volume()
#' @export
summarize_videos_in_volume <- function(vol_id = 4, vb = FALSE) {

  # Error handling
  if (length(vol_id) > 1) {
    stop("vol_id must have length 1.")
  }
  if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
    stop("vol_id must be an integer > 0.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  if (vb) message(paste0('Retrieving list of video assets for volume ', vol_id))
  vids <- list_assets_by_type(vol_id, type='video')

  if (is.null(vids)) {
    if (vb) message(paste0('No videos found in volume ', vol_id))
    return (NULL)
  } else {
    if (vb) message(paste0('Sumarizing videos in volume ', vol_id))
    vids_df <- dplyr::select(vids, asset_id, duration)
    total_vid_s <- sum(vids$duration)/1000
    total_vid_m <- total_vid_s/60
    total_vid_hrs <- total_vid_m/60
    n_vids <- dim(vids_df)[1]
    data.frame(vol_id, n_vids, total_vid_hrs)
  }
}
