#' Downloads session spreadsheet as a CSV.
#'
#' @param type Type of Databrary report to run "institutions", "people", "data"
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A tibble (data.frame) with the requested data.
#' @examples get_db_stats()
#' @examples get_db_stats("stats")
#' @examples get_db_stats("people")
#' @examples get_db_stats("places")
#' @export
get_db_stats <- function(type = "stats", vb = FALSE) {
  # Error handling
  if (length(type) > 1) {
    stop("type must have length == 1.")
  }
  if (!is.character(type)) {
    stop("'type' must be character.")
  }
  if (!(type %in% c("institutions", "places", "people", "datasets", "volumes", "stats", "numbers"))){
    stop("Type '", type, "' not valid.")
  }
  if (length(vb) > 1) {
    stop("vb must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("vb must have logical value.")
  }

  activity.api.url <- "https://nyu.databrary.org/api/activity"

  r = httr::GET(activity.api.url)
  if (vb) {
    message(paste0("Sending GET to ", activity.api.url))
  }
  if (httr::status_code(r) == 200){
    c <- jsonlite::fromJSON(httr::content(r, 'text', encoding = "UTF-8"))
    if (is.null(c)) {
      message("No content returned.")
      d <- NULL
    } else {
      if (type == "people") {
        d <- dplyr::filter(tibble::as_data_frame(c$activity$party), !is.na(id), is.na(institution))
        # d <- dplyr::filter(c$activity$party, !is.na(id))
      }
      if (type %in% c("institutions", "places")) {
        d <- dplyr::filter(tibble::as_data_frame(c$activity$party), !is.na(id), !is.na(institution))
      }
      if (type %in% c("datasets", "volumes")) {
        d <- dplyr::filter(tibble::as_data_frame(c$activity$volume), !is.na(id))
      }
      if (type %in% c("stats", "numbers")) {
        d <- data.frame(date = Sys.time(),
               investigators = c$stats$authorized[5],
               affiliates = c$stats$authorized[4],
               institutions = c$stats$authorized[6],
               datasets_total = c$stats$volumes,
               datasets_shared = c$stats$shared,
               n_files = c$stats$assets,
               hours = c$stats$duration/(1000*60*60))
        # TB = c$stats$bytes/(1e12) seems incorrect
      }
    }
    return(d)
  } else {
    message(paste0('Download Failed, HTTP status ', httr::status_code(r)))
    return(NULL)
  }
}
