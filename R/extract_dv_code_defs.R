#' Extracts the code definitions from a Datavyu (.opf) file.
#'
#' @param in_dir Input directory. Defaults to '.'
#' @param dv_fn Full Datavyu file name. Defaults to 'db'
#' @param out_dir Output directory. Defaults to in_dir
#' @param out_fn Full output file name.
#' @param auto_write_over A Boolean value. If TRUE, new output file overwrites old.
#' @param code_regex A string specifying the regular expression to extract a code.
#' @param code_vals_regex A string specifying the regular expression to extract a code value.
#' @param code_type_regex A string specifying the regular expression for the code type.
#' @param vb A boolean value. If TRUE, provides verbose output.
#' @examples
#' extract_dv_code_defs()
#' @export
extract_dv_code_defs <- function(in_dir = '.', dv_fn = 'db',
                                 out_dir = in_dir,
                                 out_fn = paste0(out_dir, "/", 'codes.csv'),
                                 auto_write_over = TRUE,
                                 code_regex = "^([a-zA-Z_]+[0-9]*[a-zA-Z_]*[0-9]*)",
                                 code_vals_regex = "\\)-([a-zA-Z\\/]+)\\|",
                                 code_type_regex = "([a-zA-Z]+)$",
                                 vb = FALSE) {
  # Check parameters -----------------------------------------------------------------
  if (!is.character(dv_fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!file.exists(paste0(in_dir, "/", dv_fn))) {
    stop(paste0("File ", dv_fn, " cannot be found."))
  }
  if (!is.character(out_fn)) {
    stop("Output file name must be a string.")
  }
  if (file.exists(out_fn)) {
    if (!auto_write_over) {
      replace.out <- readline(prompt=paste0("Output file ", out_fn, " exists. Replace (y/n)?: "))
      if (replace.out %in% c('n', 'N')) {
        stop(paste0("File ", out_fn, "not altered."))
      }
      if (vb) message(paste0("File ", out_fn, " will be replaced."))
    }
  }
  if (!is.logical(auto_write_over)) {
    stop("auto_write_over must be a logical/Boolean value.")
  }
  if (!is.character(code_regex)) {
    stop("code_regex must be character.")
  }
  if (!is.character(code_vals_regex)) {
    stop("code_vals_regex must be character.")
  }
  if (!is.character(code_type_regex)) {
    stop("code_type_regex must be character.")
  }
  if (!is.logical(vb)) {
    stop("vb must be a logical/Boolean value.")
  }

  # Open Datavyu file ------------------------------------------------------------------
  con.in <- file(paste0(in_dir, "/", dv_fn), "r")
  if (!con.in) {
    stop(paste0("Unable to open file: ", paste0(in_dir, "/", dv_fn)))
  }
  dv <- readLines(con.in)
  close(con.in)
  if (vb) message(paste0(length(dv), " lines read from file ", paste0(in_dir, "/", dv_fn)))

  # Write output line by line ------------------------------------------------------------
  dv_fl <- list.files(in_dir, '\\.opf$', full.names = TRUE)
  if (is.null(dv_fl)) stop("No Datavyu files in ", in_dir)
  if (length(dv_fl) > 1) {
    stop(paste0('More than one Datavyu file in ', in_dir, ". Source undetermined."))
  } else {
    out_fn <- paste0(out_dir, "/", tools::file_path_sans_ext(basename(dv_fl[1])), "-code-defs.csv")
  }
  con.out <- file(out_fn, "w")
  if (!con.out) {
    stop(paste0("Unable to open file: ", out_fn))
  }

  outlines <- 0
  writeLines("code,code_vals,code_type", con.out)
  for (l in 1:length(dv)) {
    if (stringr::str_detect(dv[l], code_regex)) {
      # stringr::str_match returns capture group(s) in column 2+
      code <- stringr::str_match(dv[l], code_regex)[2]
      code.vals <- stringr::str_match(dv[l], code_vals_regex)[2]
      code.type <- stringr::str_match(dv[l], code_type_regex)[2]
      writeLines(paste(code, code.vals, code.type, sep=","), con = con.out)
      outlines <- outlines + 1
    }
  }

  # Cleanup -------------------------------------------------------------------------------
  close(con.out)
  if (vb) message(paste0(outlines, " lines written to file: ", out_fn))
  return(out_fn)
}

#======================================================================================
# Convenience function to return a data frame
extract_dv_code_defs_df <- function(in_dir = '.', dv_fn = 'db',
                                    out_dir = in_dir,
                                    out_fn = paste0(out_dir, "/", 'codes.csv'),
                                    auto_write_over = TRUE,
                                    code_regex = "^([a-zA-Z_]+[0-9]*[a-zA-Z_]*[0-9]*)",
                                    code_vals_regex = "\\)-([a-zA-Z\\/]+)\\|",
                                    code_type_regex = "([a-zA-Z]+)$",
                                    vb = FALSE) {

  dv_csv <- extract_dv_code_defs(in_dir = in_dir, dv_fn = dv_fn,
                                 out_dir = out_dir,
                                 out_fn = out_fn,
                                 auto_write_over = TRUE,
                                 vb = vb)
  read.csv(dv_csv)
}
