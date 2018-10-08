#' Extracts the code definitions from a Datavyu (.opf) file.
#'
#' @param in.dir Input directory. Defaults to '.'
#' @param dv.fn Full Datavyu file name. Defaults to 'db'
#' @param out.dir Output directory. Defaults to in.dir
#' @param out.fn Full output file name.
#' @param auto.write.over A Boolean value. If TRUE, new output file overwrites old.
#' @param code.regex A string specifying the regular expression to extract a code.
#' @param code.vals.regex A string specifying the regular expression to extract a code value.
#' @param code.defs.regex A string specifying the regular expression for a code definition.
#' @param code.type.regex A string specifying the regular expression for the code type.
#' @param vb A boolean value. If TRUE, provides verbose output.
#' @examples
#' extract_dv_code_defs()
#' @export
extract_dv_code_defs <- function(in.dir = '.', dv.fn = 'db',
                                 out.dir = in.dir,
                                 out.fn = paste0(out.dir, "/", 'codes.csv'),
                                 auto.write.over = TRUE,
                                 code.regex = "^([a-zA-Z_]+[0-9]*[a-zA-Z_]*[0-9]*)",
                                 code.vals.regex = "\\)-([a-zA-Z\\/]+)\\|",
                                 code.type.regex = "([a-zA-Z]+)$",
                                 vb = FALSE) {
  # Check parameters -----------------------------------------------------------------
    if (!is.character(dv.fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!file.exists(paste0(in.dir, "/", dv.fn))) {
    stop(paste0("File ", dv.fn, " cannot be found."))
  }
  if (!is.character(out.fn)) {
    stop("Output file name must be a string.")
  }
  if (file.exists(out.fn)) {
    if (!auto.write.over) {
      replace.out <- readline(prompt=paste0("Output file ", out.fn, " exists. Replace (y/n)?: "))
      if (replace.out %in% c('n', 'N')) {
        stop(paste0("File ", out.fn, "not altered."))
      }
      if (vb) message(paste0("File ", out.fn, " will be replaced."))
    }
  }
  if (!is.logical(auto.write.over)) {
    stop("auto.write.over must be a logical/Boolean value.")
  }
  if (!is.character(code.regex)) {
    stop("code.regex must be character.")
  }
  if (!is.character(code.vals.regex)) {
    stop("code.vals.regex must be character.")
  }
  if (!is.character(code.type.regex)) {
    stop("code.type.regex must be character.")
  }
  if (!is.logical(vb)) {
    stop("vb must be a logical/Boolean value.")
  }

  # Open Datavyu file ------------------------------------------------------------------
  con.in <- file(paste0(in.dir, "/", dv.fn), "r")
  if (!con.in) {
    stop(paste0("Unable to open file: ", paste0(in.dir, "/", dv.fn)))
  }
  dv <- readLines(con.in)
  close(con.in)
  if (vb) message(paste0(length(dv), " lines read from file ", paste0(in.dir, "/", dv.fn)))

  # Write output line by line ------------------------------------------------------------
  dv_fl <- list.files(in.dir, '\\.opf$', full.names = TRUE)
  if (is.null(dv_fl)) stop("No Datavyu files in ", in.dir)
  if (length(dv_fl) > 1) {
    stop(paste0('More than one Datavyu file in ', in.dir, ". Source undetermined."))
  } else {
    out.fn <- paste0(out.dir, "/", tools::file_path_sans_ext(basename(dv_fl[1])), "-code-defs.csv")
  }
  con.out <- file(out.fn, "w")
  if (!con.out) {
    stop(paste0("Unable to open file: ", out.fn))
  }

  outlines <- 0
  writeLines("code,code_vals,code_type", con.out)
  for (l in 1:length(dv)) {
    if (stringr::str_detect(dv[l], code.regex)) {
      # stringr::str_match returns capture group(s) in column 2+
      code <- stringr::str_match(dv[l], code.regex)[2]
      code.vals <- stringr::str_match(dv[l], code.vals.regex)[2]
      code.type <- stringr::str_match(dv[l], code.type.regex)[2]
      writeLines(paste(code, code.vals, code.type, sep=","), con = con.out)
      outlines <- outlines + 1
    }
  }

  # Cleanup -------------------------------------------------------------------------------
  close(con.out)
  if (vb) message(paste0(outlines, " lines written to file: ", out.fn))
  }
