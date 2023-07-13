#' Retrieve Party (Person/Institution) Info as Data Frame.
#'
#' @param party_id Party number to retrieve information about.
#' @param convert_JSON A Boolean value if TRUE converts the JSON download.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @returns A data frame with information about the selected party.
#' @examples
#' get_party_as_df() # Info about Rick Gilmore.
#' get_party_as_df(party_id = 8) # Info about NYU.
#' @export
get_party_as_df <- function(party_id = 6,
                           convert_JSON = TRUE,
                           vb = FALSE) {

  # Error handling
  if (length(party_id) > 1) {
    stop("party_id must be single value")
  }
  if ((!is.numeric(party_id)) || (party_id <= 0)) {
    stop("party_id must be an integer > 0")
  }
  if ((!is.logical(convert_JSON))) {
    stop("convert_JSON must be a logical value")
  }
  if ((!is.logical(vb))) {
    stop("vb must be a logical value")
  }

  r <- GET_db_contents(URL_components = paste('/api/party', party_id, sep='/'), vb=vb,
                       convert_JSON = convert_JSON)
  if (!is.null(r)) {
    as.data.frame(r)
  } else {
    if (vb) message("No party data returned for party ", party_id)
    r
  }
}
