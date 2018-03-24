#' Lists basic information about people on Databrary.
#'
#' @param people.list Party number(s) for people to list.
#' @return Status code if successful.
#' @examples
#' list_people()
list_people <- function(people.list = 7, vb = FALSE) {
  # Creates a list of Databrary people as data.frame
  #
  # Args:
  #  people.list: list or array of party indices. Default is 7.
  #
  # Returns:
  #  Data frame with institutional information

  # Error handling
  if (sum((people.list < 0))) {
    stop("Person indices must be > 0")
  }

  # Get one institution's data
  list_people <- function(party) {
    if (is_person(party)) {
      download_party(party)
    }
  }

  if (length(people.list) == 1) {
    as.data.frame(list_people(people.list))
  } else {
    l <- sapply(people.list, list_people)
    Reduce(function(x,y) merge(x,y, all=TRUE), l[-which(sapply(l, is.null))])
  }
}
