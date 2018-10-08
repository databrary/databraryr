#' Extracts the elements from a raw (*.opf) Datavyu coding file.
#'
#' @param in.dir Input directory
#' @param in.fn File name for the Datavyu file.
#' @param out.dir Output directory to save exported files.
#' @param auto.write.over A Boolean value. If TRUE, any existing files in out.dir are overwritten.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return The output directory where the extracted files were saved.
#' @examples
#' extract_dv()
#' @export
extract_dv <- function(in.dir = '.',
                       in.fn = list.files(in.dir, '\\.opf$', full.names = TRUE),
                       out.dir = in.dir,
                       auto.write.over = TRUE,
                       vb = FALSE) {
  # Parameter checking -----------------------------------------------------------------
  if (!is.character(in.fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!file.exists(in.fn)) {
    stop(paste0("File ", in.fn, " not found.", "\n"))
  }
  if (!(tools::file_ext(in.fn) == "opf")) {
    stop(paste0("File ", in.fn, " does not have Datavyu (.opf) extension."))
  }
  if (length(in.fn) > 1) {
    stop(paste0("Multiple Datavyu files in ", in.dir))
  }
  if (!is.null(out.dir)) {
    if (!is.character(out.dir)) {
      stop(paste0("Output directory name must be a string."))
    }
    if (!dir.exists(out.dir)) {
        if (vb) message(paste0("Creating directory ", out.dir, "."))
        dir.create(out.dir)
    }
  } else {
    # Create new directory in . based on in.fn if none supplied
    if (vb) message(paste0("No output directory supplied. Creating from input file name."))
    out.dir <- tools::file_path_sans_ext(basename(in.fn))
    if (dir.exists(out.dir)) {
      yn <- readline(prompt = paste0("Output directory ", out.dir, " exists. Extract here (y/n): "))
      if (yn %in% c("N", "n")) {
        stop(paste0("Directory ", out.dir, " unaltered."))
      }
    } else {
      if (vb) message(paste0("Creating directory ", out.dir, "\n"))
      dir.create(out.dir)
    }
  }
  if (!is.logical(auto.write.over)) {
    stop("auto.write.over must be logical.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  # Extract file and return --------------------------------------------------------------
  if (vb) message(paste0("Extracting file ", in.fn, " to ", out.dir))
  utils::unzip(in.fn, exdir = out.dir)

  # Return out.dir for chaining ----------------------------------------------------------
  if (vb) message(paste0("Success. File extracted to ", out.dir, "/."))
  return(out.dir)
}
