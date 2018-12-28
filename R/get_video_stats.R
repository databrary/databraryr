#' Reports summary statistics about the videos available from a given volume.
#'
#' @param vol_id Target volume number.
#' @param vb A boolean value.
#' @return Data frame with summary statistics about the videos in the volume.
#' @examples
#' get_video_stats()
#' @export
get_video_stats <- function(vol_id = 1, vb = FALSE) {
  # test parameters --------------------------------------------------------
  if (!is.numeric(vol_id)) {
    stop("Volume must be numeric.")
  }
  if (vol_id < 1) {
    stop("Volume must be >= 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must be Boolean.")
  }

  # get list of videos in volume -------------------------------------------
  vids_df <- list_assets_by_type(vol_id = vol_id, type = "video", vb = vb)
  if (is.null(vids_df)) {
    if (vb) message(paste0("No videos found in volume ", vol_id, ".\n"))
    return(NULL)
  }

  # get session metadata from volume ---------------------------------------
  demo_df <- download_session_csv(vol_id = vol_id, vb = vb)
  if (is.null(demo_df)) {
    if (vb) message(paste0("No session spreadsheet found in volume ", vol_id, ".\n"))
    return(NULL)
  }

  # create and return data frame -------------------------------------------
  if (vb) message(paste0("Creating data frame for data from volume ", vol_id))
  m <- dplyr::left_join(vids_df, demo_df, by = "session.id")
  data.frame(vol_id = vol_id,
             n_videos = length(unique(m$asset.id)),
             n_sessions = length(unique(m$session.id)),
             tot_hrs = sum(m$duration)/(1000*60*60))
}
