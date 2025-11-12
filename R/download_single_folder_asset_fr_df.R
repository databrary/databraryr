#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download a Single Folder Asset From a Data Frame Row.
#'
#' @description
#' Helper used by `download_folder_assets_fr_df()` to fetch a single asset via
#' the signed-download workflow.
#'
#' @param i Integer. Index of the asset within `folder_df`.
#' @param folder_df Data frame containing folder asset metadata.
#' @param target_dir Base directory for downloads.
#' @param add_folder_subdir Logical. When `TRUE`, creates a subdirectory per
#'   folder inside `target_dir`.
#' @param overwrite Logical. When `FALSE`, existing files are saved with a
#'   timestamped suffix.
#' @param make_portable_fn Logical. When `TRUE`, filenames are sanitized via
#'   `make_fn_portable()`.
#' @param timeout_secs Numeric. Timeout applied to the signed download request.
#' @param rq Optional `httr2` request object reused to request signed links.
#'
#' @returns Path to the downloaded asset or `NULL` if the download fails.
#'
#' @inheritParams options_params
#'
#' @export
download_single_folder_asset_fr_df <- function(i = NULL,
                                               folder_df = NULL,
                                               target_dir = tempdir(),
                                               add_folder_subdir = TRUE,
                                               overwrite = TRUE,
                                               make_portable_fn = FALSE,
                                               timeout_secs = REQUEST_TIMEOUT_VERY_LONG,
                                               vb = options::opt("vb"),
                                               rq = NULL) {
  assertthat::assert_that(length(i) == 1)
  assertthat::is.number(i)
  assertthat::assert_that(i > 0)

  assertthat::assert_that(is.data.frame(folder_df))
  required_cols <- c("vol_id", "folder_id", "asset_id", "asset_name")
  missing_cols <- setdiff(required_cols, names(folder_df))
  if (length(missing_cols) > 0) {
    stop(
      "folder_df is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  assertthat::assert_that(length(target_dir) == 1)
  assertthat::is.string(target_dir)
  assertthat::assert_that(dir.exists(target_dir) || dir.create(target_dir, recursive = TRUE, showWarnings = FALSE))
  assertthat::is.writeable(target_dir)

  assertthat::assert_that(length(add_folder_subdir) == 1)
  assertthat::assert_that(is.logical(add_folder_subdir))

  assertthat::assert_that(length(overwrite) == 1)
  assertthat::assert_that(is.logical(overwrite))

  assertthat::assert_that(length(make_portable_fn) == 1)
  assertthat::assert_that(is.logical(make_portable_fn))

  assertthat::is.number(timeout_secs)
  assertthat::assert_that(length(timeout_secs) == 1)
  assertthat::assert_that(timeout_secs > 0)

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

  assertthat::assert_that(is.null(rq) || ("httr2_request" %in% class(rq)))

  this_asset <- folder_df[i, , drop = FALSE]
  if (nrow(this_asset) == 0) {
    if (vb) {
      message("No asset for index ", i)
    }
    return(NULL)
  }

  dest_dir <- if (isTRUE(add_folder_subdir)) {
    file.path(target_dir, this_asset$folder_id)
  } else {
    target_dir
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  assertthat::assert_that(dir.exists(dest_dir))
  assertthat::is.writeable(dest_dir)

  base_name <- this_asset$asset_name
  if (is.null(base_name) || is.na(base_name) || base_name == "") {
    base_name <- paste0("asset-", this_asset$asset_id)
  }

  extension <- ""
  if ("format_extension" %in% names(this_asset)) {
    ext_value <- this_asset$format_extension
    if (!is.null(ext_value) && !is.na(ext_value) && nzchar(ext_value)) {
      if (tools::file_ext(base_name) != ext_value) {
        extension <- paste0(".", ext_value)
      }
    }
  }

  candidate_name <- paste0(base_name, extension)

  if (make_portable_fn) {
    if (vb) {
      message("Making file name '", candidate_name, "' portable.")
    }
    candidate_name <- make_fn_portable(candidate_name, vb = vb)
  }

  dest_file <- file.path(dest_dir, candidate_name)
  if (file.exists(dest_file) && !overwrite) {
    if (vb) {
      message("Generating new unique (time-stamped) file name.")
    }
    candidate_name <- paste0(
      this_asset$folder_id,
      "-",
      this_asset$asset_id,
      "-",
      format(Sys.time(), "%F-%H%M-%S"),
      ifelse(
        nzchar(tools::file_ext(candidate_name)),
        paste0(".", tools::file_ext(candidate_name)),
        ""
      )
    )
    dest_file <- file.path(dest_dir, candidate_name)
  }

  download_folder_asset(
    vol_id = this_asset$vol_id,
    folder_id = this_asset$folder_id,
    asset_id = this_asset$asset_id,
    file_name = candidate_name,
    target_dir = dest_dir,
    timeout_secs = timeout_secs,
    vb = vb,
    rq = rq
  )
}



