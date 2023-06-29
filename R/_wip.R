#-------------------------------------------------------------------------------
# Functions that are still works-in-progress

#-------------------------------------------------------------------------------
summarize_demo_part_w_video <- function(vol_id = 4, vb = FALSE) {
  # Error checking ----------------------------------------------------------
  if (!is.numeric(vol_id)) {
    stop("Volume must be numeric.")
  }
  if (vol_id < 1) {
    stop("Volume must be >= 1.")
  }
  if (!is.logical(vb)) {
    stop("vb type must be logical.")
  }
  if (length(vb) > 1) {
    stop("vb must have length = 1.")
  }
  
  # Gather video assets in volume -------------------------------------------
  if (vb) message("")
  vids_df <- list_assets_by_type(vol_id = vol_id, type = "video")
  if (is.null(vids_df)) {
    stop(paste0("No videos found in volume ", vol_id, ".\n"))
  }
  
  # Gather demographic data from spreadsheet --------------------------------
  demo_df <- download_session_csv(vol_id = vol_id)
  
  if (is.null(demo_df)) {
    stop(paste0("No session spreadsheet found in volume ", vol_id, "."))
  }
  
  # Merge video data with demographic data and return data frame
  m <- dplyr::left_join(vids_df, demo_df, by = "session_id")
  if (is.null(m)) {
    if (vb) message("No videos that match volume sessions.")
    NULL
  } else {
    # Light reordering of fields
    dplyr::select(m, c(8, 9, 1, 2:7, 11:32))
  }
}
