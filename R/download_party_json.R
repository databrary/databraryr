#' Lists basic information about people on Databrary.
#'
#' @param party Party number to retrieve information about.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return JSON file with data.
#' @examples
#' download_party()
download_party_json <- function(party = 6,
                           vb = FALSE) {

  # Error handling
  if (length(party) > 1) {
    stop("Party must be single value")
  }
  if ((!is.numeric(party)) || (party <= 0)) {
    stop("Party must be an integer > 0")
  }

  if (!exists("databrary_config_status")) {
    config_db(vb = vb)
  }
  #authenticate_db()

  # Assemble URL, GET(), and handle response
  party.url <- paste0(databrary.url, "/api/party/", party)
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
