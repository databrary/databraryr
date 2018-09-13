#' Downloads container and record structure from Databrary volume.
#'
#' @param vol.id Databrary volume number.
#' @param convert.JSON Boolean.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return List of containers and records from the specified volume.
#' @examples
#' downloade_containers_records()
#' @export
download_containers_records <- function(vol.id = 2, convert.JSON = TRUE,
                                        vb = FALSE) {

  # Error handling
  # TODO(ROG): vectorize
  if (length(vol.id) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(vol.id)) || (vol.id <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  # if ((!exists("databrary_config_status")) || (!databrary_config_status)){
  #   config_db(vb = vb)
  # }
  #authenticate_db(vb = vb)

  url.cont.rec <- paste0("https://nyu.databrary.org/api/volume/", vol.id, "?", "containers&records")
  if (vb) message(paste0("Sending GET to ", url.cont.rec))
  g = httr::GET(url.cont.rec)
  if (httr::status_code(g) == 200) {
    g.content <- httr::content(g, 'text', encoding = "UTF-8")
    if(convert.JSON) {
      return(jsonlite::fromJSON(g.content))
    } else {
      return(g.content)
    }
  } else if (vb) message( paste( 'Download Failed, HTTP status ', httr::status_code(g), '\n', sep="" ) )
}
