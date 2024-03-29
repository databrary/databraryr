% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_volume_owners.R
\name{list_volume_owners}
\alias{list_volume_owners}
\title{List Owners of a Databrary Volume.}
\usage{
list_volume_owners(vol_id = 1, vb = FALSE, rq = NULL)
}
\arguments{
\item{vol_id}{Selected volume number. Default is volume 1.}

\item{vb}{A boolean value. If TRUE provides verbose output.}

\item{rq}{An \code{httr2} request object. If NULL (the default)
a request will be generated, but this will only permit public information
to be returned.}
}
\value{
A data frame with information about a volume's owner(s).
}
\description{
List Owners of a Databrary Volume.
}
\examples{
\donttest{
list_volume_owners() # Lists information about the owners of volume 1.
}
}
