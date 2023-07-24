#' Returns the avatar (image) for a given person.
#'
#' @param party_id Party number to retrieve information about. Default is 6
#' (Rick Gilmore).
#' @param show_person_info Show the person's name and affiliation in the output.
#' Default is TRUE.
#' @param vb A Boolean value if TRUE returns verbose output. Default is TRUE.
#' @returns An image file.
#' @examples
#' download_party_avatar() # Show Rick Gilmore's (party 6) avatar.
#' @export
download_party_avatar <- function(party_id = 6,
                                  show_person_info = TRUE,
                                  vb = FALSE) {
  # Error handling
  if (length(party_id) > 1) {
    stop("party_id must be single value")
  }
  if ((!is.numeric(party_id)) || (party_id <= 0)) {
    stop("party_id must be an integer > 0")
  }
  if (!is.logical(show_person_info))  {
    stop("show_person_info must be a logical value")
  }
  if (!is.logical(vb)) {
    stop("vb must be a logical value")
  }

  a <- GET_db_contents(
    base_URL = "https://nyu.databrary.org",
    URL_components = paste0('/party/', party_id, '/avatar'),
    vb = vb,
    convert_JSON = FALSE
  )
  
  if (show_person_info) {
    r <- download_party(party_id)
    if (is.list(r)) {
      person_str <-
        paste0(r$prename, " ", r$sortname, ", ", r$affiliation)
      message(person_str)          
    } else {
     message("Unable to extract info for party '", party_id, "'.") 
    }
  }
  a
}
