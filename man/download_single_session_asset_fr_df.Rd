% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_single_session_asset_fr_df.R
\name{download_single_session_asset_fr_df}
\alias{download_single_session_asset_fr_df}
\title{Download Single Asset From Databrary}
\usage{
download_single_session_asset_fr_df(
  i = NULL,
  session_df = NULL,
  target_dir = tempdir(),
  add_session_subdir = TRUE,
  overwrite = TRUE,
  make_portable_fn = FALSE,
  timeout_secs = REQUEST_TIMEOUT_VERY_LONG,
  vb = options::opt("vb"),
  rq = NULL
)
}
\arguments{
\item{i}{An integer. Index into a row of the session asset data frame.
Default is NULL.}

\item{session_df}{A row from a data frame from \code{list_session_assets()}
or \code{list_volume_assets()}. Default is NULL>}

\item{target_dir}{A character string. Directory to save the downloaded file.
Default is a temporary directory given by a call to \code{tempdir()}.}

\item{add_session_subdir}{A logical value. Add add the session name to the
file path so that files are in a subdirectory specific to the session. Default
is TRUE.}

\item{overwrite}{A logical value. Overwrite an existing file. Default is TRUE.}

\item{make_portable_fn}{A logical value. Replace characters in file names
that are not broadly portable across file systems. Default is FALSE.}

\item{timeout_secs}{An integer. The seconds an httr2 request will run before
timing out. Default is 600 (10 min). This is to handle very large files.}

\item{vb}{Show verbose messages. (Defaults to \code{FALSE}, overwritable using option 'databraryr.vb' or environment variable 'R_DATABRARYR_VB')}

\item{rq}{A list in the form of an \code{httr2} request object. Default is NULL.}
}
\value{
Full file name to the asset or NULL.
}
\description{
Databrary stores file types (assets) of many types. This
function downloads an asset based on its system-unique integer identifer
(asset_id) and system-unique session (slot) identifier (session_id). It
is designed to work with download_session_assets_fr_df() so that multiple
files can be downloaded simultaneously.
}
\examples{
\donttest{
\dontrun{
vol_1 <- list_session_assets(session_id = 9807)
a_1 <- vol_1[1,]
tmp_dir <- tempdir()
fn <- file.path(tmp_dir, paste0(a_1$asset_name, ".", a_1$format_extension))
download_single_session_asset_fr_df(a_1$asset_id,
  fn,
  session_id = a_1$session_id,
  vb = TRUE)

}
}
}
