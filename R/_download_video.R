#========================================================================================
#' Download a specific video file.
#'
#' @param session_id Slot/session number.
#' @param asset_id Asset number.
#' @param segment_id Video segment.
#' @param out_dir Directory to save output file.
#' @param file_name Name for downloaded file, default is 'test.mp4'.
#' @param return_response A Boolean value.
#' @param return_metadata Return video duration and format data via package `av`.
#' Default is TRUE.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_video()
download_video <- function(session_id = 9807,
                           asset_id = 1,
                           segment_id = "-",
                           out_dir = '.',
                           file_name = "test.mp4",
                           return_response = FALSE,
                           report_metadata = TRUE,
                           vb = FALSE) {
  # Parameter checking ----------------------------------------------------------------
  if (length(session_id) > 1) {
    stop("Session ID must have length 1.")
  }
  if ((!is.numeric(session_id)) || session_id <= 0) {
    stop("Session ID must be a number > 0.")
  }
  if (length(asset_id) > 1) {
    stop("Asset ID must have length 1.")
  }
  if ((!is.numeric(asset_id)) || asset_id <= 0) {
    stop("Asset must be number > 0.")
  }
  if (length(segment_id) > 1) {
    stop("Segment ID must have length 1.")
  }
  if (!(is.character(segment_id))) {
    stop(
      "Segment ID must be a character string, '-' for the entire video or '<start_ms>,<end_ms>' for a segment"
    )
  }
  if (length(out_dir) > 1) {
    stop("`out_dir` must have length 1.")
  }
  if (!(is.character(out_dir))) {
    stop("`out_dir` must be a character string.")
  }
  if (length(file_name) > 1) {
    stop("File name must have length 1.")
  }
  if (!is.character(file_name)) {
    stop("File name must be character string.")
  }
  if (!is.logical(return_response)) {
    stop("`return_response` must be logical value.")
  }
  if (length(return_response) > 1) {
    stop("`return_response` must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("`vb` must be logical value.")
  }
  if (length(vb) > 1) {
    stop("`vb` must have length == 1.")
  }
  
  video_duration_ms = 1
  if (vb)
    message('Getting file duration.')
  g <-
    httr::GET(paste0("https://nyu.databrary.org/api/asset/", asset_id))
  if (httr::status_code(g) == 200) {
    if (vb) {
      message("Successful HTML GET query.")
    }
    file_info <-
      jsonlite::fromJSON(httr::content(g, type = 'text', encoding = 'utf8'))
    if (file_info$format == "-800") {
      video_duration_ms = file_info$duration
      if (vb)
        message(paste0('File duration: ', video_duration_ms, ' ms.'))
    }
  } else {
    if (vb)
      message(paste0('Download Failed, HTTP status ', httr::status_code(g)))
    return (NULL)
  }
  
  segment_id <-
    validate_segment_id(segment_id, video_duration_ms, vb = vb)
  
  url_download <-
    paste0(
      "https://nyu.databrary.org",
      paste(
        "/slot",
        session_id,
        segment_id,
        "asset",
        asset_id,
        "download",
        sep = "/"
      )
    )
  
  # Add segment id to file name, but replace - with 'all' and comma with -
  if (segment_id == '-') {
    segment_id <- "all"
  } else {
    segment_id <-
      stringr::str_replace(segment_id, pattern = ",", replacement = "-")
  }
  
  if (vb)
    message(paste0('Sending GET to ', url_download))
  message(paste0(
    "Downloading video asset ",
    asset_id,
    " from session ",
    session_id
  ))
  webpage <- httr::GET(url_download, httr::progress())
  if (webpage$status_code == 200) {
    content.type <- webpage$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query.")
      message(paste0("Content-type is ", content.type))
    }
    if (content.type == "video/mp4") {
      if (file_name == "test.mp4") {
        if (vb) {
          if (vb)
            message("File name unspecified. Generating unique name.")
        }
        file_name <- paste0(
          out_dir,
          "/sess_",
          session_id,
          "-file_",
          asset_id,
          "-",
          segment_id,
          "-",
          format(Sys.time(), "%F-%H%M-%S"),
          ".mp4"
        )
      }
      if (vb) {
        if (vb)
          message(paste0("Downloading video file as: \n", file_name))
      }
      bin <- httr::content(webpage, 'raw')
      writeBin(bin, file_name)
      message(paste0("Downloaded '", file_name, "'."))
      if (return_response)
        return(out_dir)
      if (report_metadata) {
        if (requireNamespace("av", quietly = TRUE)) {
          vd <- av::av_video_info(file_name)
          message(
            paste0(vd$video$width, "x", vd$video$height, " pixels"),
            ", ",
            vd$video$codec,
            ", ",
            vd$video$frames,
            " fr @ ",
            vd$video$framerate,
            " fps."
          )
        } else {
          message("Package `av` not installed. Skipping metadata report.")
        }
      }
    }
  } else {
    if (vb)
      message(paste0('Download Failed, HTTP status ', webpage$status_code))
    if (return_response)
      return(NULL)
  }
}

#========================================================================================
#' Download all videos in a session and save to a local directory.
#'
#' @param session_id Session number.
#' @param out_dir Directory to save output file.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_vids_in_session()
#' @export
download_vids_in_session <- function(session_id = 9803,
                                     out_dir = ".",
                                     vb = FALSE) {
  # Parameter checking ----------------------------------------------------------------
  if (!is.numeric(session_id))
    stop("`session_id` must be numeric.")
  if (!is.character(out_dir))
    stop("`out_dir` must be character string.")
  if (!dir.exists(out_dir))
    stop("Output directory '", out_dir, "' does not exist.")
  if (!is.logical(vb))
    stop("`vb` must be logical value.")
  
  # List assets, filter videos, download ----------------------------------------------
  these_assets <- list_assets_in_session(session_id)
  if (!is.null(these_assets)) {
    these_vids <- dplyr::filter(these_assets, mimetype == "video/mp4")
    if (!is.null(these_vids)) {
      message(paste0(
        "There are ",
        dim(these_vids)[1],
        " videos in session ",
        session_id,
        "."
      ))
      mapply(download_video,
             session_id,
             these_vids$asset_id,
             out_dir)
    } else {
      if (vb)
        message(paste0("No videos in session ", session_id, "."))
      NULL
    }
  } else {
    if (vb)
      message(paste0("No assets in session ", session_id, "."))
    NULL
  }
}

#========================================================================================
#' Download all videos in a volume and save to a local directory.
#'
#' @param session_id Volume number.
#' @param out_dir Directory to save output file.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_vids_in_volume()
#' @export
download_vids_in_volume <-
  function(vol_id = 31,
           out_dir = ".",
           vb = FALSE) {
    # Parameter checking ----------------------------------------------------------------
    if (!is.numeric(vol_id))
      stop("`vol_id` must be numeric.")
    if (!is.character(out_dir))
      stop("`out_dir` must be character string.")
    if (!dir.exists(out_dir))
      stop("Output directory '", out_dir, "' does not exist.")
    if (!is.logical(vb))
      stop("`vb` must be logical value.")
    
    # List sessions in volume, download videos in each
    these_sessions <- databraryapi::list_sessions(vol_id)
    if (!is.null(these_sessions)) {
      sess_ids <- unique(these_sessions$session_id)
      if (length(sess_ids > 0)) {
        mapply(download_vids_in_session, sess_ids, out_dir)
      } else {
        if (vb)
          message("No unique sessions")
      }
    } else {
      if (vb)
        message(paste0("No sessions in volume ", vol_id, "."))
      NULL
    }
  }

#========================================================================================
#' Download all videos in a session and save to a local directory.
validate_segment_id <- function(segment_id,
                                video_duration_ms,
                                segment_regex = "([0-9]+),([0-9]+)",
                                vb = FALSE) {
  if (stringr::str_detect(segment_id, pattern = segment_regex)) {
    seg_matches <-
      stringr::str_match(segment_id, pattern = "([0-9]+),([0-9]+)")
    start_ms_s <- seg_matches[2]
    end_ms_s <- seg_matches[3]
    start_ms <- as.numeric(start_ms_s)
    end_ms <- as.numeric(end_ms_s)
    
    if (start_ms < 0) {
      if (vb)
        message(paste0("start_ms < 0; changing to 0"))
      start_ms_s <- "0"
    }
    if (end_ms < 0) {
      if (vb)
        message("end_ms > duration or < 0; converting to duration.")
      end_ms_s = as.character(video_duration_ms)
    }
    return(paste0(start_ms_s, ",", end_ms_s))
  } else if (stringr::str_detect(segment_id, pattern = "-")) {
    return(segment_id)
  } else {
    if (vb)
      message("Invalid segment ID, returning valid default value of '-'")
    return("-")
  }
}
