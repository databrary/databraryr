#' Lists basic information about people on Databrary.
#'
#' @param people_list Party number(s) for people to list.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return A data frame with the people in the specified range of party ID's.
#' @examples
#' list_people()
#' @export
list_people <- function(people_list = 5:8, vb = FALSE) {
  # Error handling -----------------------------------------------------------
  if (!is.numeric(people_list)) {
    stop("people_list must be numeric.")
  }
  if (sum((people_list < 0))) {
    stop("Person indices must be > 0")
  }
  if (!is.logical(vb)) {
    stop("vb must be Boolean.")
  }
  if (length(is.logical) > 1) {
    stop("vb must be single value.")
  }

  # Get one institution's data
  list_person <- function(party) {
    if (is_person(party, vb = vb)) {
      p <- download_party(party, vb = vb)
      # Drop parties without names
      if (is.null(p$sortname)) {
        return(NULL)
      } else {
        return(p)
      }
    }
  }

  if (length(people_list) == 1) {
    if (vb) message("Only one ID in list.")
    as.data.frame(list_person(people_list))
  } else {
    if (vb) message("Multiple person IDs in list.")
    purrr::map_df(people_list, list_person, .progress="People info:")
  }
}
