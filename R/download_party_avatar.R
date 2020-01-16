#' Returns the avatar (image) for a given person.
#'
#' @param party_id Party number to retrieve information about.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return Image file.
#' @examples
#' download_party_avatar()
#' @export
download_party_avatar <- function(party_id = 6,
                           vb = FALSE) {

  # Error handling
  if (length(party_id) > 1) {
    stop("party_id must be single value")
  }
  if ((!is.numeric(party_id)) || (party_id <= 0)) {
    stop("party_id must be an integer > 0")
  }

  r <- GET_db_contents(base_URL = "https://nyu.databrary.org",
                       URL_components = paste0('/party/', party_id, '/avatar'), vb=vb,
                       convert_JSON = FALSE)
  r
}
