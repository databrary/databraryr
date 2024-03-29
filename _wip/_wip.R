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


#' Download a specific Datavyu (.opf) file.
#'
#' @param vol_id Volume ID
#' @param session_id Slot/session number.
#' @param asset_id Asset number.
#' @param out_dir Directory to save output.
#' @param file_name Name for downloaded file.
#' @param return_response A Boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_datavyu()
download_datavyu <- function(vol_id = 1, session_id = 9807,
                             asset_id = 117035,
                             out_dir = ".",
                             file_name = "test.opf",
                             return_response = FALSE,
                             vb = FALSE) {
  # Check parameters -----------------------------------------------------------------------
  if (!is.numeric(vol_id)) {
    stop("vol_id must be numeric.")
  }
  if (vol_id < 1) {
    stop("vol_id must be >= 1.")
  }
  if (!is.character(out_dir)) {
    stop("out_dir must be character.")
  }
  if (!is.character(file_name)) {
    stop("file_name must be a string.")
  }
  if (!is.null(out_dir)) {
    if (!is.character(out_dir)) {
      stop(paste0("Output directory name must be a string."))
    }
    if (dir.exists(out_dir)) {
      yn <- readline(prompt = paste0("Output directory '", out_dir, "/' exists. Extract here (y/n): "))
      if (yn %in% c("N", "n")) {
        stop(paste0("Directory ", out_dir, " unaltered."))
      }
    } else {
      if (vb) message(paste0("Creating directory ", out_dir, "/"))
      dir.create(out_dir)
    }
  } else {
    # Create new directory in . based on file_name if none supplied
    if (vb) message(paste0("No output directory supplied. Creating from input file name."))
    out_dir <- tools::file_path_sans_ext(basename(file_name))
    if (dir.exists(out_dir)) {
      yn <- readline(prompt = paste0("Output directory '", out_dir, "/' exists. Extract here (y/n): "))
      if (yn %in% c("N", "n")) {
        stop(paste0("Directory '", out_dir, "' unaltered."))
      }
    } else {
      if (vb) message(paste0("Creating directory '", out_dir, "'.\n"))
      dir.create(out_dir)
    }
  }
  
  # List Datavyu files in volume, then download first one-----------------------------------
  # TODO(ROG): Add support for multiple files
  # dv_df <- list_assets_by_type(vol_id = vol_id, type = "datavyu")
  dv_df <- list_specified_assets_in_session(session_id = session_id,
                                            media_type = 'Datavyu')
  if (is.null(dv_df)) {
    message(paste0("No files of type ", type, "in volume ", vol_id))
  } else {
    url.download <- paste0("https://nyu.databrary.org",
                           paste("/slot", session_id, "-", "asset", asset_id,
                                 "download", sep="/"))
    webpage <- httr::GET(url.download)
    if (webpage$status_code == 200) {
      content.type <- webpage$headers$`content-type`
      if (vb) {
        message("Successful HTML GET query.")
        message(paste0("Content-type is '", content.type, "'."))
      }
      if (content.type == "application/vnd.datavyu") {
        if (file_name == "test.opf") {
          if (vb) {
            if (vb) message("File name unspecified. Generating unique name.")
          }
          file_name <- paste0(out_dir, "/vol_", vol_id, "-sess_",
                              session_id, "-asset_",
                              asset_id, "-",
                              format(Sys.time(), "%F-%H%M-%S"), ".opf")
        }
        if (vb) {
          if (vb) message(paste0("Downloading Datavyu file as: \n", file_name))
        }
        bin <- httr::content(webpage, 'raw')
        writeBin(bin, file_name)
        return(file_name)
      }
    } else {
      if (vb) message(paste0('Download Failed, HTTP status ', webpage$status_code))
      return(NULL)
    }
  }
}

