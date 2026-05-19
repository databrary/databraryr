# Items related to use of the options package.
options::define_options(
  "Show verbose messages.",
  vb = FALSE
)

#' @eval options::as_roxygen_docs()
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.name = "Rick Gilmore",
    devtools.desc.author = "Rick Gilmore <rog1@psu.edu> [aut, cre]",
    devtools.desc.license = "MIT",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if (any(toset)) options(op.devtools[toset])

  base_url <- Sys.getenv("DATABRARY_BASE_URL", "https://api.databrary.org")
  ns <- asNamespace(pkgname)

  for (nm in c("DATABRARY_BASE_URL", "OAUTH_TOKEN_URL", "OAUTH_TEST_URL", "LOGIN")) {
    unlockBinding(nm, ns)
  }
  assign("DATABRARY_BASE_URL", base_url, envir = ns)
  assign("OAUTH_TOKEN_URL", sprintf("%s/o/token/", base_url), envir = ns)
  assign("OAUTH_TEST_URL", sprintf("%s/oauth2/test/", base_url), envir = ns)
  assign("LOGIN", sprintf("%s/login/", base_url), envir = ns)
  for (nm in c("DATABRARY_BASE_URL", "OAUTH_TOKEN_URL", "OAUTH_TEST_URL", "LOGIN")) {
    lockBinding(nm, ns)
  }

  invisible()
}

utils::globalVariables(".data")

# Internal: nullable API scalars — tibble/map_dfr drop raw NULL columns.
null_to_na_double <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_real_)
  }
  v <- suppressWarnings(as.numeric(x))
  if (length(v) == 0L || all(is.na(v))) NA_real_ else v[[1L]]
}

null_to_na_character <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  tc <- suppressWarnings(as.character(x))
  if (length(tc) == 0L) NA_character_ else tc[[1L]]
}
