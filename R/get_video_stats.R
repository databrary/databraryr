#' Report Stats About Videos in a Volume.
#'
#'#' @description
#' `r lifecycle::badge("superseded")`
#' `get_video_stats()` has been superseded in favor of `summarize_videos_in_volume()`.
#' 
#' @param this_vol_id Target volume number.
#' @param vb A boolean value.
#' @returns A data frame with summary statistics about the videos in the volume.
#' @examples
#' \donttest{
#' \dontrun{
#' get_video_stats() # Retrieves summary statistics about videos in volume 2.
#' }
#' }
#' @export
get_video_stats <- function(this_vol_id = 2, vb = FALSE) {

  # Check parameters
  assertthat::assert_that(length(this_vol_id) == 1)
  assertthat::assert_that(is.numeric(this_vol_id))
  assertthat::assert_that(this_vol_id >= 1)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))  # Error handling
  
  # get list of videos in volume -------------------------------------------
  vids_df <- databraryr::list_assets_by_type(vol_id = this_vol_id, type = "video", vb = vb)
  if (is.null(vids_df)) {
    if (vb) message(paste0("No videos found in volume ", this_vol_id, ".\n"))
    return(NULL)
  } else {
    vids_df <- dplyr::mutate(vids_df, vol_id = this_vol_id)
  }

  # get session metadata from volume ---------------------------------------
  sess_df <- databraryr::get_session_as_df(vol_id = this_vol_id, vb = vb)
  if (is.null(sess_df)) {
    if (vb) message(paste0("No session spreadsheet found in volume ", this_vol_id, ".\n"))
    return(NULL)
  }

  # create and return data frame -------------------------------------------
  if (vb) message(paste0("Creating data frame for data from volume ", this_vol_id))
  data.frame(vol_id = this_vol_id,
             n_vids = length(unique(vids_df$asset_id)),
             n_sess = length(unique(sess_df$session_id)),
             tot_vid_hrs = sum(vids_df$duration)/(1000*60*60))
}

