#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download a Video Asset via Signed URL.
#'
#' @param vol_id Volume identifier containing the session.
#' @param session_id Session identifier containing the asset.
#' @param asset_id Asset identifier for the video file.
#' @param file_name Optional explicit file name. Defaults to the API-provided
#'   value.
#' @param target_dir Directory to save the downloaded file. Defaults to
#'   `tempdir()`.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq Optional `httr2` request object reused when requesting the signed
#'   link.
#'
#' @returns Path to the downloaded video or `NULL` on failure.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' download_video() # Default public video from volume 1
#' download_video(vol_id = 1, session_id = 9825, asset_id = 11643,
#'                file_name = "rdk.mp4")
#' }
#' }
#'
#' @export
download_video <- function(vol_id = 1,
                           session_id = 9807,
                           asset_id = 1,
                           file_name = NULL,
                           target_dir = tempdir(),
                           vb = options::opt("vb"),
                           rq = NULL) {
  assertthat::assert_that(length(asset_id) == 1)
  assertthat::assert_that(is.numeric(asset_id))
  assertthat::assert_that(asset_id >= 1)
  
  assertthat::assert_that(length(session_id) == 1)
  assertthat::assert_that(is.numeric(session_id))
  assertthat::assert_that(session_id >= 1)
  
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)
  
  if (!is.null(file_name)) {
    assertthat::assert_that(length(file_name) == 1)
    assertthat::assert_that(is.character(file_name))
    if (!endsWith(tolower(file_name), ".mp4")) {
      stop("file_name must end with '.mp4' when provided.", call. = FALSE)
    }
  }
  
  assertthat::assert_that(length(target_dir) == 1)
  assertthat::assert_that(is.character(target_dir))
  assertthat::assert_that(
    dir.exists(target_dir) ||
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  )
  assertthat::is.writeable(target_dir)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  assertthat::assert_that(is.null(rq) ||
                            ("httr2_request" %in% class(rq)))
  
  download_session_asset(
    vol_id = vol_id,
    session_id = session_id,
    asset_id = asset_id,
    file_name = file_name,
    target_dir = target_dir,
    vb = vb,
    rq = rq,
    timeout_secs = REQUEST_TIMEOUT_VERY_LONG
  )
}
