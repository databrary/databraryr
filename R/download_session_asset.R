#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download an Asset via Signed Link.
#'
#' @description
#' Databrary serves assets through short-lived, signed URLs. This helper
#' requests the signed link for a session asset and streams the file to the
#' requested directory.
#'
#' @param vol_id Integer. Volume identifier. Default is 1.
#' @param session_id Integer. Session identifier. Default is 9807.
#' @param asset_id Integer. Asset identifier within the session. Default is 1.
#' @param file_name Optional character string. Target file name. Defaults to the
#'   API-provided file name.
#' @param target_dir Character string. Directory where the file will be saved.
#'   Default is `tempdir()`.
#' @param timeout_secs Numeric. Timeout (seconds) applied to the download
#'   request. Default is `REQUEST_TIMEOUT`.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is `NULL`, in which case a
#'   default authenticated request is generated.

#'
#' @returns The path to the downloaded file (character string) or `NULL` if the
#'   download fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' download_session_asset() # Default public asset in volume 1
#' download_session_asset(vol_id = 1, session_id = 9825, asset_id = 11643,
#'                        file_name = "rdk.mp4")
#' }
#' }
#' @export
download_session_asset <- function(vol_id = 1,
                                   session_id = 9807,
                                   asset_id = 1,
                                   file_name = NULL,
                                   target_dir = tempdir(),
                                   timeout_secs = REQUEST_TIMEOUT,
                                   vb = options::opt("vb"),
                                   rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  
  assertthat::assert_that(length(asset_id) == 1)
  assertthat::assert_that(is.numeric(asset_id))
  assertthat::assert_that(asset_id >= 1)
  
  if (!is.null(file_name)) {
    assertthat::assert_that(length(file_name) == 1)
    assertthat::assert_that(is.character(file_name))
  }
  
  assertthat::assert_that(length(target_dir) == 1)
  assertthat::assert_that(is.character(target_dir))
  assertthat::assert_that(dir.exists(target_dir))
  
  assertthat::is.number(timeout_secs)
  assertthat::assert_that(length(timeout_secs) == 1)
  assertthat::assert_that(timeout_secs > 0)
  
  validate_flag(vb, "vb")
  
  assertthat::assert_that(is.null(rq) ||
                            ("httr2_request" %in% class(rq)))
  
  path <- sprintf(API_FILES_DOWNLOAD_LINK, vol_id, session_id, asset_id)
  link <- request_signed_download_link(path = path, rq = rq, vb = vb)
  
  if (is.null(link)) {
    return(NULL)
  }
  
  resolved_name <- if (!is.null(file_name)) {
    file_name
  } else if (!is.null(link$file_name)) {
    link$file_name
  } else {
    paste0(session_id,
           "-",
           asset_id,
           "-",
           format(Sys.time(), "%F-%H%M-%S"),
           ".bin")
  }
  
  dest_path <- file.path(target_dir, resolved_name)
  
  if (file.exists(dest_path)) {
    dest_path <- file.path(target_dir,
                           paste0(
                             tools::file_path_sans_ext(resolved_name),
                             "-",
                             format(Sys.time(), "%F-%H%M-%S"),
                             ifelse(
                               nzchar(tools::file_ext(resolved_name)),
                               paste0(".", tools::file_ext(resolved_name)),
                               ""
                             )
                           ))
  }
  
  download_signed_file(
    download_url = link$download_url,
    dest_path = dest_path,
    timeout_secs = timeout_secs,
    vb = vb
  )
}
