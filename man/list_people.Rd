% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_people.R
\name{list_people}
\alias{list_people}
\title{List Info About People.}
\usage{
list_people(people_list = 5:7, vb = FALSE)
}
\arguments{
\item{people_list}{Party number(s) for people to list.}

\item{vb}{A Boolean value if TRUE returns verbose output.}
}
\value{
A data frame with information about the people in the specified
range of party ID's.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function has been deprecated and may be removed in a future release.
See \code{get_party_by_id()} for similar functionality.
}
\examples{
\donttest{
list_people() # Lists people with party IDs 5:7, Databrary's founders
#' }

}
