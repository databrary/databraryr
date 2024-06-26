#' List Info About People.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated and may be removed in a future release.
#' See `get_party_by_id()` for similar functionality.
#'
#' @param people_list Party number(s) for people to list.
#' @param vb A Boolean value if TRUE returns verbose output.
#' 
#' @returns A data frame with information about the people in the specified
#'   range of party ID's.
#'   
#' @examples
#' \donttest{
#' list_people() # Lists people with party IDs 5:7, Databrary's founders
#' #' }
#' 
#' @export
list_people <- function(people_list = 5:7, vb = FALSE) {

  # Check parameters
  assertthat::assert_that(length(people_list) != 0)
  assertthat::assert_that(is.numeric(people_list))

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Get one party's data
  list_person <- function(party_id) {
    if (is_person(party_id = party_id, vb = vb)) {
      p <- download_party(party_id = party_id, vb = vb)
      # Drop parties without names
      if (is.null(p$sortname)) {
        return(NULL)
      } else {
        return(p)
      }
    }
  }
  
  if (length(people_list) == 1) {
    if (vb)
      message("Only one ID in list.")
    as.data.frame(list_person(people_list))
  } else {
    if (vb)
      message("Multiple person IDs in list.")
    purrr::map_df(people_list, list_person, .progress = "People info:")
  }
}
