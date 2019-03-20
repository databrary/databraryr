#' Downloads container and record structure from Databrary volume.
#' This function is deprecated, as it duplicates list_containers_records().
#'
#' @param vol_id Databrary volume number.
#' @param vb A boolean value. If TRUE provides verbose output.
#' @return List of containers and records from the specified volume.
#' @examples
#' download_containers_records()
#' @export
download_containers_records <- function(vol_id = 1,
                                        vb = FALSE) {

  list_containers_records(vol_id = vol_id, vb = vb)
  # # Error handling
  # # TODO(ROG): vectorize
  # if (length(vol_id) > 1) {
  #   stop("vol_id must have length 1.")
  # }
  # if ((!is.numeric(vol_id)) || (vol_id <= 0)) {
  #   stop("vol_id must be an integer > 0.")
  # }
  # if (!is.logical(convert_JSON)) {
  #   stop("convert_JSON must be logical")
  # }
  # if (length(convert_JSON) > 1) {
  #   stop("convert_JSON must have length == 1.")
  # }
  # if (!is.logical(vb)) {
  #   stop("vb must be logical")
  # }
  # if (length(vb) > 1) {
  #   stop("vb must have length == 1.")
  # }
  #
  # # if ((!exists("databrary_config_status")) || (!databrary_config_status)){
  # #   config_db(vb = vb)
  # # }
  # #authenticate_db(vb = vb)
  #
  # r <- GET_db_contents(URL_components = paste0('volume/', vol_id, '?containers&records'),
  #                      vb = vb, convert_JSON = convert_JSON)
  # return(r)
  #
  # # url_cont_rec <- paste0("https://nyu.databrary.org/api/volume/", vol_id, "?", "containers&records")
  # # if (vb) message(paste0("Sending GET to ", url_cont_rec))
  # # g = httr::GET(url_cont_rec)
  # # if (httr::status_code(g) == 200) {
  # #   g_content <- httr::content(g, 'text', encoding = "UTF-8")
  # #   if(convert_JSON) {
  # #     return(jsonlite::fromJSON(g_content))
  # #   } else {
  # #     return(g_content)
  # #   }
  # # } else if (vb) message( paste( 'Download Failed, HTTP status ', httr::status_code(g), '\n', sep="" ) )
}
