# Items related to use of the options package.
options::define_options(
  "Show verbose messages.",
  vb = TRUE,
  "Be quiet in messaging",
  quiet = TRUE
)

#' @eval options::as_roxygen_docs()
NULL

#' @eval options::as_params()
#' @name options_params

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

