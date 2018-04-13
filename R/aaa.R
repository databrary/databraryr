.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    # devtools.path = "~/R-dev",
    # devtools.install.args = "",
    devtools.name = "Rick Gilmore",
    devtools.desc.author = "Rick Gilmore <rick.gilmore@databrary.org> [aut, cre]",
    devtools.desc.license = "MIT",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}
