extract_dv <- function(in.fn, out.dir = NULL,
                       auto.write.over = TRUE,
                       vb = FALSE) {
  # Error handling
  if (!is.character(in.fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!file.exists(in.fn)) {
    stop(paste0("File ", in.fn, " not found.", "\n"))
  }
  if (!(tools::file_ext(in.fn) == "opf")) {
    stop(paste0("File ", in.fn, " does not have Datavyu (.opf) extension."))
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
      if (vb) message(paste0("Creating directory ", out.dir, "\n"))
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

  if (vb) message(paste0("Extracting file ", in.fn, " to ", out.dir, "\n"))
  unzip(in.fn, exdir = out.dir)

  # Return out.dir for chaining
  return(out.dir)
}
