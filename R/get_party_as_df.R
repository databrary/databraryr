#' Retrieve Party (Person/Institution) Info as Data Frame.
#'
#' @param party_id Party number to retrieve information about.
#' @param convert_JSON A Boolean value if TRUE converts the JSON download.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @returns A data frame with information about the selected party.
#' @examples
#' \donttest{
#' get_party_as_df() # Info about Rick Gilmore.
#' get_party_as_df(party_id = 8) # Info about NYU.
#' #' }
#' @export
get_party_as_df <- function(party_id = 6,
                           convert_JSON = TRUE,
                           vb = FALSE) {

  # Check parameters
  assertthat::assert_that(length(party_id) == 1)
  assertthat::assert_that(is.numeric(party_id))
  assertthat::assert_that(party_id >= 1)

  assertthat::assert_that(length(convert_JSON) == 1)
  assertthat::assert_that(is.logical(convert_JSON))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  r <- GET_db_contents(URL_components = paste('/api/party', party_id, sep='/'), vb=vb,
                       convert_JSON = convert_JSON)
  if (!is.null(r)) {
    as.data.frame(r)
  } else {
    if (vb) message("No party data returned for party ", party_id)
    r
  }
}
