.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    # devtools.path = "~/R-dev",
    # devtools.install.args = "",
    devtools.name = "Rick Gilmore",
    devtools.desc.author = "Rick Gilmore <rog1@psu.edu> [aut, cre]",
    devtools.desc.license = "MIT",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

utils::globalVariables(".data")

source("R/make_default_request.R")
source("R/CONSTANTS.R")
rq <- make_default_request()