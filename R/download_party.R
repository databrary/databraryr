#' Lists basic information about people on Databrary.
#'
#' @param party.id Party number to retrieve information about.
#' @param convert.JSON A Boolean value if TRUE converts the JSON download
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return Status code if successful.
#' @examples
#' download_party()
#' @export
download_party <- function(party.id = 6,
                           convert.JSON = TRUE,
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
  party.url <- paste0("https:/nyu.databrary.org/api/party/", party.id)
  if (vb) {
    message(paste0("Sending GET to ", party.url, "\n"))
  }
  r = httr::GET(party.url)

  if (httr::status_code(r) == 200) {
    r.content <- httr::content(r, 'text', encoding = 'UTF-8')
    if(convert.JSON) {
      return(jsonlite::fromJSON(r.content))
    } else {
      return(r.content)
    }
  } else {
    if (vb) {
      message(paste0('Download Failed, HTTP status ', httr::status_code(r), "\n"))
    }
    return(r)
  }
}
