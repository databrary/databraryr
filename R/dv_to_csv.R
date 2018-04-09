#' Converts a Datavyu (.opf) file to CSV.
#'
#' @param dv.dir Directory to extracted Datavyu file
#' @param dv.fn Datavyu code file. Defaults to 'db'.
#' @param out.fn Output file name. Default is based on dv.dir.
#' @param auto.write.over A Boolean value. If TRUE, new output file overwrites old.
#' @examples
#' dv_to_csv()
dv_to_csv <- function(dv.dir, dv.fn = "db", out.fn = paste0(dv.dir, ".csv"), auto.write.over = FALSE) {
  if (!is.character(dv.dir)) {
    stop("Datavyu directory must be a string.")
  }
  if (!dir.exists(dv.dir)) {
    stop(paste0("Directory ", dv.dir, " not found.\n"))
  }
  if (!is.character(dv.fn)) {
    stop("Datavyu file name must be a string.")
  }
  if (!is.character(out.fn)) {
    stop("Output file name must be a string.")
  }
  if (!is.logical(auto.write.over)) {
    stop("auto.write.over must be a logical/Boolean value.")
  }
  if (file.exists(paste0(dv.dir, "/", out.fn))) {
    if (!auto.write.over) {
      replace.out <- readline(prompt=paste0("Output file ", out.fn, " exists. Replace (y/n)?: "))
      if (replace.out %in% c('n', 'N')) {
        stop(paste0("File ", out.fn, "not altered."))
      }
      message(paste0("File ", out.fn, " will be replaced."))
    }
  }

  # Open Datavyu file
  con.in <- file(paste0(dv.dir, "/", dv.fn), "r")
  if (!con.in) {
    stop(paste0("Unable to open file: ", dv.fn))
  }
  dv <- readLines(con.in)
  close(con.in)
  message(paste0(length(dv), " lines read from file ", dv.fn))

  con.out <- file(paste0(dv.dir, "/", out.fn), "w")
  if (!con.out) {
    stop(paste0("Unable to open file: ", out.fn))
  }

  # Write output file
  onset.offset.regex <- "^([0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{3},[0-9]{2}:[0-9]{2}:[0-9]{2}:[0-9]{3})"
  code.values.regex <- "\\([a-zA-Z ?,.'/0-9;!|~`]+\\)$"
  code.regex <- "^([a-zA-Z_]+)"
  outlines <- 0

  writeLines("code,onset,offset,code.value", con.out)
  code <- "-"
  code.values <- "-,-"
  times <- "-,-"
  for (l in 1:length(dv)) {
    # If not a valid first column skip row
    if (!(stringr::str_detect(dv[l], code.regex)) &&
        !(stringr::str_detect(dv[l], onset.offset.regex))) {
      next
    }
    # If a valid code definition row
    if (stringr::str_detect(dv[l], code.regex)) {
      code <- stringr::str_extract(dv[l], code.regex)
      next
    }
    # If a valid code row, process
    if (stringr::str_detect(dv[l], onset.offset.regex)) {
      times <- stringr::str_extract(dv[l], onset.offset.regex)
      if (stringr::str_detect(dv[l], code.values.regex)) {
        code.values <- paste0('"', stringr::str_extract(dv[l], code.values.regex), '"')
      } else {
        code.values <- "-"
      }
    } else {
      times <- "-,-"
    }
    writeLines(paste(code, times, code.values, sep=","), con = con.out)
    outlines <- outlines + 1
  }

  # Cleanup
  close(con.out)
  message(paste0(outlines, " lines written to file: ", out.fn))
  }
