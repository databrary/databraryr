#' Downloads container and record structure from Databrary volume.
#'
#' @param volume Databrary volume number.
#' @param convert.JSON Boolean.
#' @param vb Boolean.
#' @return List of containers and records from the specified volume.
#' @examples
#' downloade_containers_records()
download_containers_records <- function(volume = 2, convert.JSON = TRUE,
                                        vb = FALSE) {
  # Downloads container and record structure for a given Databrary volume
  # Converts from JSON to a native R structure if requested.
  #
  # Args:
  #  volume: The Databrary volume (integer) to download. Default is 2.
  #  url.base: The base URL for the Databrary volume API. Default is "https://nyu.databrary.org/api/volume"
  #  convert.JSON: Flag specifying whether to convert JSON to R data structure. Default is TRUE.
  #  vb: Flag specifying whether to provide vb status messages. Default is FALSE.
  #
  # Returns:
  #  The containers and record structure for a specified volume as JSON or as an R list.

  # Error handling
  if (length(volume) > 1) {
    stop("Volume must have length 1.")
  }
  if ((!is.numeric(volume)) || (volume <= 0)) {
    stop("Volume must be an integer > 0.")
  }

  if ((!exists("databrary_config_status")) || (!databrary_config_status)){
    config_db(vb=vb)
  }
  #authenticate_db(vb=vb)

  url.cont.rec <- paste(vol.api.url, "/", volume, "?", "containers&records", sep="")

  g = httr::GET(url.cont.rec)

  if (httr::status_code(g) == 200){
    g.content <- httr::content(g, 'text', encoding = "UTF-8")
    if(convert.JSON) {
      return(jsonlite::fromJSON(g.content))
    } else {
      return(g.content)
    }
  } else if (vb) cat( paste( 'Download Failed, HTTP status ', httr::status_code(g), '\n', sep="" ) )
}
