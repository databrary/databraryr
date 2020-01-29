#' Lists sponsors for a given Databrary party (institution or person).
#'
#' @param party_id Target volume number.
#' @param vb A Boolean value. If TRUE provides verbose output.
#' @return A tibble (data.frame) with the requested data.
#' @examples
#' get_sponsors() # Default is Rick Gilmore (party 6)
#' @export
get_sponsors <- function(party_id = 6, vb = FALSE) {
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
    p <- g$parents$party
    if (!is.null(p)) {
      p
    } else {
      NULL
    }
  } else {
    if (vb)
      message(paste0("No data for party ", party_id, "."))
    NULL
  }
}
