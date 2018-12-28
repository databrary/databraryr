#' Download a specific video file.
#'
#' @param vol_id Volume ID.
#' @param session_id Slot/session number.
#' @param asset_id Asset number.
#' @param segment_id Video segment.
#' @param out_dir Directory to save output file.
#' @param file_name Name for downloaded file.
#' @param return_response A Boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_video()
#' @export
download_video <- function(vol_id = 1,
                           session_id = 9807,
                           asset_id = 1,
                           segment_id = "-",
                           out_dir = '.',
                           file_name = "test.mp4",
                           return_response=FALSE, vb=FALSE) {
  # Parameter checking ----------------------------------------------------------------
  if (length(asset_id) > 1) {
    stop("Asset ID must have length 1.")
  }
  if ((!is.numeric(asset_id)) || asset_id <= 0 ) {
    stop("Asset must be number > 0.")
  }
  if (length(session_id) > 1) {
    stop("Session ID must have length 1.")
  }
  if ((!is.numeric(session_id)) || session_id <= 0 ) {
    stop("Session ID must be a number > 0.")
  }
  if (!(is.character(segment_id))) {
    stop("Segment ID must be a character string, '-' for the entire video or '<start_ms>,<end_ms>' for a segment")
  }
  if (!is.character(file_name)) {
    stop("File name must be character string.")
  }

  video_duration_ms = 1
  if (vb) message('Getting file duration.')
  g <- httr::GET(paste0("https://nyu.databrary.org/api/asset/", asset_id))
  if (httr::status_code(g) == 200) {
    if (vb) {
      message("Successful HTML GET query.")
    }
    file_info <- jsonlite::fromJSON(httr::content(g, type = 'text', encoding = 'utf8'))
    if (file_info$format == "-800") {
      video_duration_ms = file_info$duration
      if (vb) message(paste0('File duration: ', video_duration_ms, ' ms.'))
    }
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', httr::status_code(g)))
    return (NULL)
  }

  segment_id <- validate_segment_id(segment_id, video_duration_ms, vb = vb)

  url.download <- paste0("https://nyu.databrary.org", paste("/slot", session_id,
                                                            segment_id, "asset",
                                                            asset_id, "download",
                                                            sep="/"))

  # Add segment id to file name, but replace - with 'all' and comma with -
  if (segment_id == '-') {
    segment_id <- "all"
  } else {
    segment_id <- stringr::str_replace(segment_id, pattern = ",", replacement = "-")
  }

  if (vb) message(paste0('Sending GET to ', url.download))
  webpage <- httr::GET(url.download)
  if (webpage$status_code == 200) {
    content.type <- webpage$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query\n")
      message(paste0("Content-type is ", content.type, ".\n"))
    }
    # TODO(somebody): Add support for other content types
    if (content.type == "video/mp4") {
      if (file_name == "test.mp4") {
        if (vb) {
          message("File name unspecified. Generating unique name.\n")
        }
        file_name <- paste0(out_dir, "/", vol_id, "-",
                            session_id, "-",
                            asset_id, "-", segment_id, "-",
                            format(Sys.time(), "%F-%H%M-%S"), ".mp4")
      }
      if (vb) message(paste0("Downloading video as ", file_name))
      bin <- httr::content(webpage, 'raw')
      writeBin(bin, file_name)
      return(out_dir)
    }
  } else {
    if (vb) message(paste('Download Failed, HTTP status ', webpage$response$status_code, '\n', sep="" ))
    if (return_response) return(webpage$response)
  }

  webpage <- httr::GET(url.download)
  if (webpage$status_code == 200) {
    content.type <- webpage$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query.")
      message(paste0("Content-type is ", content.type))
    }
    if (content.type == "video/mp4") {
      if (file_name == "test.mp4") {
        if (vb) {
          if (vb) message("File name unspecified. Generating unique name.")
        }
        file_name <- paste0(out_dir, "/", vol_id, "-",
                            session_id, "-",
                            asset_id, "-", segment_id, "-",
                            format(Sys.time(), "%F-%H%M-%S"), ".opf")
      }
      if (vb) {
        if (vb) message(paste0("Downloading video file as: \n", file_name))
      }
      bin <- httr::content(webpage, 'raw')
      writeBin(bin, file_name)
      message(paste0('Video ', file_name, ' downloaded to ', out_dir, '.'))
      return(out_dir)
    }
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', webpage$status_code))
    return(NULL)
  }
}

validate_segment_id <- function(segment_id, video_duration_ms,
                                segment.regex = "([0-9]+),([0-9]+)", vb = vb) {
  if (stringr::str_detect(segment_id, pattern = segment.regex)) {
    seg_matches <- stringr::str_match(segment_id, pattern = "([0-9]+),([0-9]+)")
    start_ms_s <- seg_matches[2]
    end_ms_s <- seg_matches[3]
    start_ms <- as.numeric(start_ms_s)
    end_ms <- as.numeric(end_ms_s)

    if (start_ms < 0) {
      if (vb) message(paste0("start_ms < 0; changing to 0"))
      start_ms_s <- "0"
    }
    if (end_ms < 0) {
      if (vb) message("end_ms > duration or < 0; converting to duration.")
      end_ms_s = as.character(video_duration_ms)
    }
    return(paste0(start_ms_s, ",", end_ms_s))
  } else if (stringr::str_detect(segment_id, pattern = "-")) {
    return(segment_id)
  } else {
    if (vb) message("Invalid segment ID, returning valid default value of '-'")
    return("-")
  }
}
