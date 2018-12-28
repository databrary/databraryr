#' Get the time range from a given video or audio asset.
#'
#' @param HHMMSSmmm a string in the format "HH:MM:SS:mmm"
#' @examples
#' HHMMSSmmm_to_ms()
#' @export
HHMMSSmmm_to_ms <- function(HHMMSSmmm = "01:01:01:333") {
  # Check parameters
  if (!is.character(HHMMSSmmm)) {
    stop("HHMMSSmmm must be a string.")
  }

  if (stringr::str_detect(HHMMSSmmm, "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]{3})")) {
    time_segs <- stringr::str_match(HHMMSSmmm, "([0-9]{2}):([0-9]{2}):([0-9]{2}):([0-9]{3})")
    as.numeric(time_segs[5]) + as.numeric(time_segs[4])*1000 + as.numeric(time_segs[3])*1000*60 +
      as.numeric(time_segs[2])*1000*60*60
  } else {
    NULL
  }
}
