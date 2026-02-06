#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download a Folder Asset via Signed Link.
#'
#' @description
#' Databrary serves folder-scoped assets through signed URLs. This helper
#' requests the signed link for a folder asset and streams the file to the
#' specified directory.
#'
#' @param vol_id Integer. Volume identifier containing the folder. Default is 1.
#' @param folder_id Integer. Folder identifier within the volume. Default is 9807,
#' the Materials folder for Volume 1.
#' @param asset_id Integer. Asset identifier within the folder. Default is 1, a
#' demo video called 'counting_demo_video.mp4'.
#' @param file_name Optional character string. File name to use when saving the
#'   asset. Defaults to the API-provided file name.
#' @param target_dir Character string. Directory where the file will be saved.
#'   Default is `tempdir()`.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An `httr2` request object. Default is `NULL`, in which case a
#'   default authenticated request is generated.
#' @param timeout_secs Numeric. Timeout (seconds) applied to the download
#'   request. Default is `REQUEST_TIMEOUT`.
#'
#' @returns The path to the downloaded file (character string) or `NULL` if the
#'   download fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' download_folder_asset() # Default public asset in folder 1 of volume 1
#' download_folder_asset(vol_id = 1, folder_id = 9807, asset_id = 1,
#'                       file_name = "video.mp4")
#' }
#' }
#'
#' @export
download_folder_asset <- function(vol_id = 1,
                                  folder_id = 9807,
                                  asset_id = 1,
                                  file_name = "video.mp4",
                                  target_dir = tempdir(),
                                  timeout_secs = REQUEST_TIMEOUT,
                                  vb = options::opt("vb"),
                                  rq = NULL) {
  assertthat::assert_that(length(vol_id) == 1)
  assertthat::assert_that(is.numeric(vol_id))
  assertthat::assert_that(vol_id >= 1)

  assertthat::assert_that(length(folder_id) == 1)
  assertthat::assert_that(is.numeric(folder_id))
  assertthat::assert_that(folder_id >= 1)

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

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  assertthat::assert_that(is.null(rq) || ("httr2_request" %in% class(rq)))

  path <- sprintf(API_FOLDER_FILE_DOWNLOAD_LINK, vol_id, folder_id, asset_id)
  link <- request_signed_download_link(path = path, rq = rq, vb = vb)

  if (is.null(link)) {
    return(NULL)
  }

  resolved_name <- if (!is.null(file_name)) {
    file_name
  } else if (!is.null(link$file_name)) {
    link$file_name
  } else {
    paste0(
      folder_id,
      "-",
      asset_id,
      "-",
      format(Sys.time(), "%F-%H%M-%S"),
      ".bin"
    )
  }

  dest_path <- file.path(target_dir, resolved_name)

  if (file.exists(dest_path)) {
    dest_path <- file.path(
      target_dir,
      paste0(
        tools::file_path_sans_ext(resolved_name),
        "-",
        format(Sys.time(), "%F-%H%M-%S"),
        ifelse(
          nzchar(tools::file_ext(resolved_name)),
          paste0(".", tools::file_ext(resolved_name)),
          ""
        )
      )
    )
  }

  download_signed_file(
    download_url = link$download_url,
    dest_path = dest_path,
    timeout_secs = timeout_secs,
    vb = vb
  )
}
