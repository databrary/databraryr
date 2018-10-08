#' Download a specific Datavyu (.opf) file.
#'
#' @param vol.id Volume ID
#' @param session.id Slot/session number.
#' @param asset.id Asset number.
#' @param out.dir Directory to save output.
#' @param file.name Name for downloaded file.
#' @param return.response A Boolean value.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @examples
#' download_datavyu()
#' @export

download_datavyu <- function(vol.id = 1, session.id = 9807,
                             asset.id = 117035,
                             out.dir = "tmp",
                             file.name = "test.opf",
                             return.response = FALSE,
                             vb = TRUE) {
  # Check parameters -----------------------------------------------------------------------
  if (!is.numeric(vol.id)) {
    stop("vol.id must be numeric.")
  }
  if (vol.id < 1) {
    stop("vol.id must be >= 1.")
  }
  if (!is.character(out.dir)) {
    stop("out.dir must be character.")
  }
  if (!is.character(file.name)) {
    stop("file.name must be a string.")
  }

  if (!is.null(out.dir)) {
    if (!is.character(out.dir)) {
      stop(paste0("Output directory name must be a string."))
    }
    if (dir.exists(out.dir)) {
      yn <- readline(prompt = paste0("Output directory ", out.dir, " exists. Extract here (y/n): "))
      if (yn %in% c("N", "n")) {
        stop(paste0("Directory ", out.dir, " unaltered."))
      }
    } else {
      if (vb) message(paste0("Creating directory ", out.dir, "/"))
      dir.create(out.dir)
    }
  } else {
    # Create new directory in . based on in.fn if none supplied
    if (vb) message(paste0("No output directory supplied. Creating from input file name."))
    out.dir <- tools::file_path_sans_ext(basename(in.fn))
    if (dir.exists(out.dir)) {
      yn <- readline(prompt = paste0("Output directory ", out.dir, "/ exists. Extract here (y/n): "))
      if (yn %in% c("N", "n")) {
        stop(paste0("Directory ", out.dir, " unaltered."))
      }
    } else {
      if (vb) message(paste0("Creating directory ", out.dir, "\n"))
      dir.create(out.dir)
    }
  }

  # List Datavyu files in volume, then download first one-----------------------------------
  # TODO(ROG): Add support for multiple files
  # dv_df <- list_assets_by_type(vol.id = vol.id, type = "datavyu")
  dv_df <- list_specified_assets_in_session(vol.id = vol.id,
                                            session.id = session.id,
                                            media.type = 'Datavyu')
  if (is.null(dv_df)) {
    message(paste0("No files of type ", type, "in volume ", vol.id))
  } else {
    url.download <- paste0("https://nyu.databrary.org",
                           paste("/slot", session.id, "-", "asset", asset.id,
                                 "download", sep="/"))
    webpage <- httr::GET(url.download)
    if (webpage$status_code == 200) {
      content.type <- webpage$headers$`content-type`
      if (vb) {
        message("Successful HTML GET query.")
        message(paste0("Content-type is ", content.type))
      }
      if (content.type == "application/vnd.datavyu") {
        if (file.name == "test.opf") {
          if (vb) {
            if (vb) message("File name unspecified. Generating unique name.")
          }
          file.name <- paste0(out.dir, "/", vol.id, "-",
                              session.id, "-",
                              asset.id, "-",
                              format(Sys.time(), "%F-%H%M-%S"), ".opf")
        }
        if (vb) {
          if (vb) message(paste0("Downloading Datavyu file as: \n", file.name))
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
}
