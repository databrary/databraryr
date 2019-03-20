#' Lists basic information about people on Databrary.
#'
#' @param party_id Party number to retrieve information about.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return JSON file with data.
#' @examples
#' download_party()
download_party_json <- function(party_id = 6,
                           vb = FALSE) {

  # Error handling
  if (length(party_id) > 1) {
    stop("party_id must be single value")
  }
  if ((!is.numeric(party_id)) || (party_id <= 0)) {
    stop("party_id must be an integer > 0")
  }

  # if (!exists("databrary_config_status")) {
  #   config_db(vb = vb)
  # }
  #authenticate_db()

  r <- GET_db_contents(URL_components = paste('party', party_id, sep='/'),
                       vb = vb, convert_JSON = FALSE)
  return(r)
  # Assemble URL, GET(), and handle response
  # party_url <- paste0("https://nyu.databrary.org/api/party/", party_id)
  # if (vb) {
  #   message(paste0("Sending GET to ", party_url, "\n"))
  # }
  # r = httr::GET(party_url)
  #
  # if (httr::status_code(r) == 200) {
  #     return(httr::content(r, 'text', encoding = 'UTF-8'))
  # } else {
  #   if (vb) {
  #     message(paste0('Download Failed, HTTP status ', httr::status_code(r), "\n"))
  #   }
  #   return(r)
  # }
}
