#' Retrieve Party (Person/Institution) Info as Data Frame.
#'
#' @param party_id Party number(s) to retrieve information about.
#' @param convert_JSON A Boolean value if TRUE converts the JSON download.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @returns A data frame with information about the selected party or parties.
#' @examples
#' \donttest{
#' get_party_as_df() # Info about Rick Gilmore.
#' 
#' get_party_as_df(party_id = 8) # Info about NYU.
#' 
#' get_party_as_df(5:7) # Info about Databrary's founders
#' #' }
#' @export
get_party_as_df <- function(party_id = 6,
                           convert_JSON = TRUE,
                           vb = FALSE) {

  # Check parameters
  assertthat::assert_that(sum(party_id >= 1) == length(party_id))
  assertthat::is.number(party_id) # Should handle next two, but does not...?
  assertthat::assert_that(!is.logical(party_id))
  assertthat::assert_that(!is.character(party_id))
  
  assertthat::assert_that(length(convert_JSON) == 1)
  assertthat::assert_that(is.logical(convert_JSON))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  #------------------------------------------------------------
  # Helper function for handling lists or arrays of party_id
  get_one_party <- function(party_id = NULL,
                            convert_JSON = TRUE,
                            vb = NULL) {
    r <- GET_db_contents(URL_components = paste('/api/party', party_id, sep='/'), vb=vb,
                         convert_JSON = convert_JSON)
    if (!is.null(r)) {
      as.data.frame(r)
    } else {
      if (vb) message("No party data returned for party ", party_id)
      r
    }
  }
  #------------------------------------------------------------
  
  if (vb) message("Retrieving info for parties: ", min(party_id), ":", max(party_id))
  purrr::map(party_id, get_one_party, vb = vb, .progress = TRUE) |>
    purrr::list_rbind()
}
