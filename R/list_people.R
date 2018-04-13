#' Lists basic information about people on Databrary.
#'
#' @param people.list Party number(s) for people to list.
#' @param vb A Boolean value if TRUE returns verbose output.
#' @return A data frame with the people in the specified range of party ID's.
#' @examples
#' list_people()
list_people <- function(people.list = 5:8, vb = FALSE) {
  # Error handling
  if (sum((people.list < 0))) {
    stop("Person indices must be > 0")
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

  if (length(people.list) == 1) {
    if (vb) message("Only one ID in list.")
    as.data.frame(list_person(people.list))
  } else {
    if (vb) message("Multiple person IDs in list.")
    l <- sapply(people.list, list_person)
    Reduce(function(x,y) merge(x,y, all=TRUE), l[-which(sapply(l, is.null))])
  }
}
