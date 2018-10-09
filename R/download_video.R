#' Download a specific video file.
#'
#' @param vol.id Volume ID.
#' @param asset.id Asset number.
#' @param session.id Slot/session number.
#' @param segment.id Video segment.
#' @param out.dir Directory to save output file.
#' @param file.name Name for downloaded file.
#' @param return.response A Boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_video()
#' @export
download_video <- function(vol.id = 1,
                           session.id = 9807,
                           asset.id = 1,
                           segment.id = "-",
                           out.dir = '.',
                           file.name = "test.mp4",
                           return.response=FALSE, vb=FALSE) {
  # Parameter checking ----------------------------------------------------------------
  if (length(asset.id) > 1) {
    stop("Asset ID must have length 1.")
  }
  if ((!is.numeric(asset.id)) || asset.id <= 0 ) {
    stop("Asset must be number > 0.")
  }
  if (length(session.id) > 1) {
    stop("Session ID must have length 1.")
  }
  if ((!is.numeric(session.id)) || session.id <= 0 ) {
    stop("Session ID must be a number > 0.")
  }
  if (!(is.character(segment.id))) {
    stop("Segment ID must be a character string, '-' for the entire video or '<start_ms>,<end_ms>' for a segment")
  }
  if (!is.character(file.name)) {
    stop("File name must be character string.")
  }

  video_duration_ms = 1
  g <- httr::GET(paste0("https://nyu.databrary.org/api/asset/", asset.id))
  if (httr::status_code(g) == 200) {
    if (vb) {
      message("Successful HTML GET query.")
    }
    file_info <- jsonlite::fromJSON(httr::content(g, type = 'text', encoding = 'utf8'))
    if (file_info$format == "-800") {
      video_duration_ms = file_info$duration
    }
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', httr::status_code(g)))
    return (NULL)
  }

  segment.id <- validate_segment_id(segment.id, video_duration_ms, vb = vb)

  url.download <- paste0("https://nyu.databrary.org", paste("/slot", session.id,
                                                            segment.id, "asset",
                                                            asset.id, "download",
                                                            sep="/"))

  # Add segment id to file name, but replace - with 'all' and comma with -
  if (segment.id == '-') {
    segment.id <- "all"
  } else {
    segment.id <- stringr::str_replace(segment.id, pattern = ",", replacement = "-")
  }

  webpage <- rvest::html_session(url.download)
  if (webpage$response$status_code == 200) {
    content.type <- webpage$response$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query\n")
      message(paste0("Content-type is ", content.type, ".\n"))
    }
    # TODO(somebody): Add support for other content types
    if (content.type == "video/mp4") {
      if (file.name == "test.mp4") {
        if (vb) {
          message("File name unspecified. Generating unique name.\n")
        }
        file.name <- paste0(out.dir, "/", vol.id, "-",
                            session.id, "-",
                            asset.id, "-", segment.id, "-",
                            format(Sys.time(), "%F-%H%M-%S"), ".mp4")
      }
      if (vb) message(paste0("Downloading video as ", file.name))
      rv <- utils::download.file(url = webpage$handle$url, destfile = file.name, mode = "wb")
      if (vb) {
        if (rv == 0) {
          message(paste0("File ", file.name, " downloaded successfully."))
        } else {
          message(paste0("File download failed."))
        }
      }
    }
  } else {
    if (vb) message(paste('Download Failed, HTTP status ', webpage$response$status_code, '\n', sep="" ))
    if (return.response) return(webpage$response)
  }

  webpage <- httr::GET(url.download)
  if (webpage$status_code == 200) {
    content.type <- webpage$headers$`content-type`
    if (vb) {
      message("Successful HTML GET query.")
      message(paste0("Content-type is ", content.type))
    }
    if (content.type == "video/mp4") {
      if (file.name == "test.mp4") {
        if (vb) {
          if (vb) message("File name unspecified. Generating unique name.")
        }
        file.name <- paste0(out.dir, "/", vol.id, "-",
                            session.id, "-",
                            asset.id, "-", segment.id, "-",
                            format(Sys.time(), "%F-%H%M-%S"), ".opf")
      }
      if (vb) {
        if (vb) message(paste0("Downloading video file as: \n", file.name))
      }
      bin <- httr::content(webpage, 'raw')
      writeBin(bin, file.name)
      return(out.dir)
    }
  } else {
    if (vb) message(paste0('Download Failed, HTTP status ', webpage$status_code))
    return(NULL)
  }
}

validate_segment_id <- function(segment.id, video_duration_ms,
                                segment.regex = "([0-9]+),([0-9]+)", vb = vb) {
  if (stringr::str_detect(segment.id, pattern = segment.regex)) {
    seg_matches <- stringr::str_match(segment.id, pattern = "([0-9]+),([0-9]+)")
    start_ms_s <- seg_matches[2]
    end_ms_s <- seg_matches[3]
    start_ms <- as.numeric(start_ms_s)
    end_ms <- as.numeric(end_ms_s)

    if ((start_ms > video_duration_ms) || (start_ms < 0)) {
      if (vb) message(paste0("start_ms < duration or < 0; changing to 0"))
      start_ms_s <- "0"
    }
    if ((end_ms > video_duration_ms) || (end_ms < 0)) {
      if (vb) message("end_ms > duration or < 0; converting to duration.")
      end_ms_s = as.character(video_duration_ms)
    }
    return(paste0(start_ms_s, ",", end_ms_s))
  } else if (stringr::str_detect(segment.id, pattern = "-")) {
    return(segment.id)
  } else {
    if (vb) message("Invalid segment ID, returning valid default value of '-'")
    return("-")
  }
}
