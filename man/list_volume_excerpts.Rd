% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_volume_excerpts.R
\name{list_volume_excerpts}
\alias{list_volume_excerpts}
\title{List Image or Video Excerpts On A Databrary Volume.}
\usage{
list_volume_excerpts(vol_id = 1, vb = options::opt("vb"), rq = NULL)
}
\arguments{
\item{vol_id}{Target volume number.}

\item{vb}{Show verbose messages. (Defaults to \code{FALSE}, overwritable using option 'databraryr.vb' or environment variable 'R_DATABRARYR_VB')}

\item{rq}{An \code{httr2} request object. Default is NULL.}
}
\value{
A list with information about any available excerpts.
}
\description{
List Image or Video Excerpts On A Databrary Volume.
}
\examples{
\donttest{
list_volume_excerpts()
}

}
