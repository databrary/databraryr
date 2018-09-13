#' Reports summary statistics about the videos available from a given volume.
#'
#' @param volume Target volume number.
#' @param vb A boolean value.
#' @return Data frame with summary statistics about the videos in the volume.
#' @examples
#' get_video_stats()
#' @export
get_video_stats <- function(volume = 1, vb = FALSE) {
  # test parameters --------------------------------------------------------
  if (!is.numeric(volume)) {
    stop("Volume must be numeric.")
  }
  if (volume < 1) {
    stop("Volume must be >= 1.")
  }

  # get list of videos in volume -------------------------------------------
  vids_df <- list_assets_by_type(volume = volume, type = "video", vb = vb)
  if (is.null(vids_df)) {
    if (vb) message(paste0("No videos found in volume ", volume, ".\n"))
    return(NULL)
  }

  # get session metadata from volume ---------------------------------------
  demo_df <- download_session_csv(volume = volume, vb = vb)
  if (is.null(demo_df)) {
    if (vb) message(paste0("No session spreadsheet found in volume ", volume, ".\n"))
    return(NULL)
  }

  # create and return data frame -------------------------------------------
  m <- dplyr::left_join(vids_df, demo_df, by = "session.id")
  data.frame(vol_id = volume,
             n_videos = length(unique(m$asset.id)),
             n_sessions = length(unique(m$session.id)),
             tot_hrs = sum(m$duration)/(1000*60*60))
}
