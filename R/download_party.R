#' Lists basic information about people on Databrary.
#'
#' @param party_id Party number to retrieve information about.
#' @param convert_JSON A Boolean value if TRUE converts the JSON download
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return Status code if successful.
#' @examples
#' download_party()
#' @export
download_party <- function(party_id = 6,
                           convert_JSON = TRUE,
                           vb = FALSE) {

  # Error handling
  if (length(party_id) > 1) {
    stop("party_id must be single value")
  }
  if ((!is.numeric(party_id)) || (party_id <= 0)) {
    stop("party_id must be an integer > 0")
  }

  r <- GET_db_contents(URL_components = paste('party', party_id, sep='/'), vb=vb, convert_JSON = convert_JSON)

  # Assemble URL, GET(), and handle response
  # party_url <- paste0("https://nyu.databrary.org/api/party/", party_id)
  # if (vb) {
  #   message(paste0("Sending GET to ", party_url, "\n"))
  # }
  # r <- httr::GET(party_url)
  #
  # if (httr::status_code(r) == 200) {
  #   r_content <- httr::content(r, 'text', encoding = 'UTF-8')
  #   if(convert_JSON) {
  #     return(jsonlite::fromJSON(r_content))
  #   } else {
  #     return(r_content)
  #   }
  # } else {
  #   if (vb) {
  #     message(paste0('Download Failed, HTTP status ', httr::status_code(r), "\n"))
  #   }
  #   return(r)
  # }
  return(r)
}
