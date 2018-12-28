#' Extracts the elements from a raw (*.opf) Datavyu coding file.
#'
#' @param in_dir Input directory
#' @param in_fn File name for the Datavyu file.
#' @param out_dir Output directory to save exported files.
#' @param auto_write_over A Boolean value. If TRUE, any existing files in out_dir are overwritten.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return The output directory where the extracted files were saved.
#' @examples
#' extract_dv()
#' @export
extract_dv <- function(in_dir = '.',
                       in_fn = list.files(in_dir, '\\.opf$', full.names = TRUE),
                       out_dir = in_dir,
                       auto_write_over = TRUE,
                       vb = FALSE) {
  # Parameter checking -----------------------------------------------------------------
  if (!is.character(in_fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!file.exists(in_fn)) {
    stop(paste0("File ", in_fn, " not found.", "\n"))
  }
  if (!(tools::file_ext(in_fn) == "opf")) {
    stop(paste0("File ", in_fn, " does not have Datavyu (.opf) extension."))
  }
  if (length(in_fn) > 1) {
    stop(paste0("Multiple Datavyu files in ", in_dir))
  }
  if (!is.null(out_dir)) {
    if (!is.character(out_dir)) {
      stop(paste0("Output directory name must be a string."))
    }
    if (!dir.exists(out_dir)) {
        if (vb) message(paste0("Creating directory ", out_dir, "."))
        dir.create(out_dir)
    }
  } else {
    # Create new directory in . based on in_fn if none supplied
    if (vb) message(paste0("No output directory supplied. Creating from input file name."))
    out_dir <- tools::file_path_sans_ext(basename(in_fn))
    if (dir.exists(out_dir)) {
      yn <- readline(prompt = paste0("Output directory ", out_dir, " exists. Extract here (y/n): "))
      if (yn %in% c("N", "n")) {
        stop(paste0("Directory ", out_dir, " unaltered."))
      }
    } else {
      if (vb) message(paste0("Creating directory ", out_dir, "\n"))
      dir.create(out_dir)
    }
  }
  if (!is.logical(auto_write_over)) {
    stop("auto_write_over must be logical.")
  }
  if (!is.logical(vb)) {
    stop("vb must be logical.")
  }

  # Extract file and return --------------------------------------------------------------
  if (vb) message(paste0("Extracting file ", in_fn, " to ", out_dir))
  utils::unzip(in_fn, exdir = out_dir)

  # Return out_dir for chaining ----------------------------------------------------------
  if (vb) message(paste0("Success. File extracted to ", out_dir, "/."))
  return(out_dir)
}
