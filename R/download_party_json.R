#' Lists basic information about people on Databrary.
#'
#' @param party.id Party number to retrieve information about.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return JSON file with data.
#' @examples
#' download_party()
#' @export
download_party_json <- function(party.id = 6,
                           vb = FALSE) {

  # Error handling
  if (length(party.id) > 1) {
    stop("party.id must be single value")
  }
  if ((!is.numeric(party.id)) || (party.id <= 0)) {
    stop("party.id must be an integer > 0")
  }

  # if (!exists("databrary_config_status")) {
  #   config_db(vb = vb)
  # }
  #authenticate_db()

  # Assemble URL, GET(), and handle response
  party.url <- paste0("https://nyu.databrary.org/api/party/", party.id)
  if (vb) {
    message(paste0("Sending GET to ", party.url, "\n"))
  }
  r = httr::GET(party.url)

  if (httr::status_code(r) == 200) {
      return(httr::content(r, 'text', encoding = 'UTF-8'))
  } else {
    if (vb) {
      message(paste0('Download Failed, HTTP status ', httr::status_code(r), "\n"))
    }
    return(r)
  }
}
