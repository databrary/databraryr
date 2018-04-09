#' Converts a Datavyu (.opf) file to CSV.
#'
#' @param dv.fn Full Datavyu file name.
#' @param out.fn Full output file name.
#' @param auto.write.over A Boolean value. If TRUE, new output file overwrites old.
#' @examples
#' dv_to_csv()
extract_dv_code_defs <- function(dv.fn, out.fn = "codes.csv", auto.write.over = TRUE) {
  if (!is.character(dv.fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!file.exists(dv.fn)) {
    stop(paste0("File ", dv.fn, " cannot be found."))
  }
  if (!is.logical(auto.write.over)) {
    stop("auto.write.over must be a logical/Boolean value.")
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
      message(paste0("File ", out.fn, " will be replaced."))
    }
  }

  # Open Datavyu file
  con.in <- file(dv.fn, "r")
  if (!con.in) {
    stop(paste0("Unable to open file: ", dv.fn))
  }
  dv <- readLines(con.in)
  close(con.in)
  message(paste0(length(dv), " lines read from file ", dv.fn))

  con.out <- file(out.fn, "w")
  if (!con.out) {
    stop(paste0("Unable to open file: ", out.fn))
  }

  # Write output file
  code.regex <- "^([a-zA-Z_]+)"
  code.defs.regex <- "[\\ a-zA-z|,-]+$"
  outlines <- 0

  writeLines("code,code.defs", con.out)

  for (l in 1:length(dv)) {
    if (stringr::str_detect(dv[l], code.regex)) {
      code <- stringr::str_extract(dv[l], code.regex)
      code.defs <- stringr::str_extract(dv[l], code.defs.regex)
      #writeLines(paste(code, code.defs, sep=","), con = con.out)
      writeLines(paste(code.defs), con = con.out)
      outlines <- outlines + 1
    }
  }

  # Cleanup
  close(con.out)
  message(paste0(outlines, " lines written to file: ", out.fn))
  }
