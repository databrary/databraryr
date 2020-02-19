#' Lists data for a given Databrary party (institution or person).
#'
#' @param party_id Target volume number.
#' @param component The specific data to retrieve for the party, c("all", "children", "parents")
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A list (or data frame) with the requested data.
#' @examples
#' get_party() # Default is New York University (party 8)
#' @export
get_party <- function(party_id = 8,
                      component = "all",
                      vb = FALSE) {
  if (length(party_id) > 1) {
    stop("'party_id' must have length == 1.")
  }
  if (!is.numeric(party_id)) {
    stop("'party_id' must be an integer.")
  }
  if (party_id < 0) {
    stop("'party_id' must be > 0.")
  }

  if (length(vb) > 1) {
    stop("'vb' must have length == 1.")
  }
  if (!is.logical(vb)) {
    stop("'vb' must be a Boolean.")
  }

  if (vb)
    message(paste0("Getting sponsors for party ", party_id, "."))
  g <-
    databraryapi::GET_db_contents(
      URL_components = paste0("/api/party/", party_id,
                              "?parents&children&access"),
      vb = vb
    )

  if (!is.null(g)) {
    if (vb)
      message(paste0("Retrieving data for party ", party_id, "."))
    if (component == "children") {
      r <- g$children
      r[[1]] # Extract from list
    } else if (component == "parents") {
      r <- g$adults # Extract from list
      r[[1]]
    } else {
      g
    }
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
