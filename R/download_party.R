#' Lists basic information about people on Databrary.
#' 
#' #' @description
#' `r lifecycle::badge("superseded")`
#' `download_party()` has been superseded in favor of `get_party_as_df()`.
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

  r <- GET_db_contents(URL_components = paste('/api/party', party_id, sep='/'), vb=vb,
                       convert_JSON = convert_JSON)
  if (!is.null(r)) {
    as.data.frame(r)
  } else {
    r
  }
}
