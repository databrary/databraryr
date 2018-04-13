#' Lists containers  and record structure from Databrary volume.
#'
#' @param volume Databrary volume number.
#' @param convert.JSON Boolean.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return JSON file with containers and records from the specified volume.
#' @examples
#' list_containers_records_json()
list_containers_records_json <- function(volume = 1,
                                        vb = FALSE) {

  # Error handling
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || (volume <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  # if ((!exists("databrary_config_status")) || (!databrary_config_status)){
  #   config_db(vb=vb)
  # }

  # Make URL, GET(), and handle response
  url.cont.rec <- paste0("https://nyu.databrary.org/api/volume/", volume, "?", "containers&records")
  if (vb) {
    message(paste0("Sending GET to ", url.cont.rec))
  }
  g = httr::GET(url.cont.rec)
  if (httr::status_code(g) == 200) {
    return(httr::content(g, 'text', encoding = "UTF-8"))
    } else {
      if (vb) {
        message(paste0( 'Download Failed, HTTP status ', httr::status_code(g)))
      }
    return(NULL)
  }
}
