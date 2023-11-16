#' Returns the Avatar(s) (images) for Authorized User(s).
#'
#'
#' @param party_id A number or range of numbers. Party number or numbers to retrieve information about. Default is 6
#' (Rick Gilmore).
#' @param show_party_info A logical value. Show the person's name and affiliation in the output.
#' Default is TRUE.
#' @param vb A Boolean value. If TRUE returns verbose output. Default is FALSE.
#' @returns An list with the avatar (image) file and a name_affil string.
#' @examples
#' \donttest{
#' \dontrun{
#' download_party_avatar() # Show Rick Gilmore's (party 6) avatar.
#'
#' # Download avatars from Databrary's founders (without name/affiliations)
#' download_party_avatar(5:7, show_party_info = FALSE)
#'
#' # Download NYU logo
#' download_party_avatar(party = 8)
#' }
#' }
#' @export
download_party_avatar <- function(party_id = 6,
                                  show_party_info = TRUE,
                                  vb = FALSE) {
  # Check parameters
  assertthat::is.number(party_id)
  assertthat::assert_that(sum(party_id >= 1) == length(party_id))
  
  assertthat::assert_that(length(show_party_info) == 1)
  assertthat::assert_that(is.logical(show_party_info))
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  #------------------------------------------------------------
  # Helper function for handling multiple queries
  get_single_avatar <- function(party_id = 6,
                                show_party_info = TRUE,
                                vb = FALSE) {
    a <- GET_db_contents(
      base_URL = "https://nyu.databrary.org",
      URL_components = paste0('/party/', party_id, '/avatar'),
      vb = vb,
      convert_JSON = FALSE
    )
    
    party_str = paste0("Databrary party ", party_id)
    if (show_party_info) {
      r <- download_party(party_id)
      if (is.list(r)) {
        if ("affiliation" %in% names(r)) {
          if (vb)
            message(party_str)
          party_str <-
            paste0(r$prename, " ", r$sortname, ", ", r$affiliation)
        } else {
          party_str <-
            paste0(r$sortname)
        }
      } else {
        message("Unable to extract info for party '", party_id, "'.")
      }
    }
    list(avatar = magick::image_read(a), name_affil = party_str)
  }
  #------------------------------------------------------------
  
  if (vb)
    message("Retrieving avatars for parties: ",
            min(party_id),
            ":",
            max(party_id))
  purrr::map(party_id, get_single_avatar, show_party_info, vb, .progress = TRUE)
}
