#' Lists basic information about people on Databrary.
#'
#' @param party_id Party number to retrieve information about.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return JSON file with data.
#' @examples
#' download_party()
download_party_json <- function(party_id = 6,
                           vb = FALSE) {

  download_party(party_id, convert_JSON = FALSE, vb)
}
