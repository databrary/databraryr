#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Download Multiple Assets From a Session Data Frame.
#'
#' @description
#' Iterates over a data frame of session assets, requesting signed download
#' links for each asset and saving them to disk. Designed to work with
#' `list_session_assets()` or `list_volume_session_assets()` output.
#'
#' @param session_df Data frame describing assets. Must include `vol_id`,
#'   `session_id`, `asset_id`, and `asset_name` columns. Defaults to the result
#'   of `list_session_assets(session_id = 9224, vol_id = 1)`. Explicit `NULL`
#'   triggers the same call using the current `vb` and `rq`.
#' @param target_dir Character string. Base directory for downloads. Defaults to
#'   `tempdir()`.
#' @param add_session_subdir Logical. When `TRUE`, creates a subdirectory per
#'   session inside `target_dir`.
#' @param overwrite Logical. When `FALSE`, the function aborts if the target
#'   directory already exists.
#' @param make_portable_fn Logical. When `TRUE`, filenames are sanitized via
#'   `make_fn_portable()`.
#' @param timeout_secs Numeric. Timeout applied to each download request.
#' @param vb Show verbose feedback. Defaults to `options::opt("vb")`.
#' @param rq An optional `httr2` request object reused when requesting signed
#'   links.
#'
#' @returns Character vector of downloaded file paths or `NULL` if the request
#'   fails before any downloads start.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' assets <- list_session_assets(vol_id = 1, session_id = 9224)
#' download_session_assets_fr_df(assets, vb = TRUE)
#' }
#' }
#' @export
download_session_assets_fr_df <-
  function(session_df = list_session_assets(session_id = 9224,
                                            vol_id = 1),
           target_dir = tempdir(),
           add_session_subdir = TRUE,
           overwrite = TRUE,
           make_portable_fn = FALSE,
           timeout_secs = REQUEST_TIMEOUT_VERY_LONG,
           vb = options::opt("vb"),
           rq = NULL) {
    assertthat::assert_that(length(target_dir) == 1)
    assertthat::assert_that(is.character(target_dir))

    assertthat::assert_that(length(add_session_subdir) == 1)
    assertthat::assert_that(is.logical(add_session_subdir))

    assertthat::assert_that(length(overwrite) == 1)
    assertthat::assert_that(is.logical(overwrite))

    assertthat::assert_that(length(make_portable_fn) == 1)
    assertthat::assert_that(is.logical(make_portable_fn))

    assertthat::assert_that(assertthat::is.number(timeout_secs))
    assertthat::assert_that(length(timeout_secs) == 1)
    assertthat::assert_that(timeout_secs > 0)

    assertthat::assert_that(length(vb) == 1)
    assertthat::assert_that(is.logical(vb))

    assertthat::assert_that(is.null(rq) ||
                              ("httr2_request" %in% class(rq)))

    if (is.null(session_df)) {
      session_df <- list_session_assets(session_id = 9224,
                                        vol_id = 1,
                                        vb = vb,
                                        rq = rq)
    }

    assertthat::assert_that(is.data.frame(session_df))
    required_cols <- c("vol_id", "session_id", "asset_id", "asset_name")
    missing_cols <- setdiff(required_cols, names(session_df))
    if (length(missing_cols) > 0) {
      stop(
        "session_df is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }

    if (dir.exists(target_dir)) {
      if (!overwrite) {
        if (vb) {
          message("`overwrite` is FALSE. Cannot continue.")
        }
        return(NULL)
      }
    } else {
      dir.create(target_dir,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
    assertthat::is.writeable(target_dir)

    if (vb) {
      message("Downloading n=", nrow(session_df), " files to ", target_dir)
    }

    purrr::map(
      seq_len(nrow(session_df)),
      download_session_asset_from_df,
      session_df = session_df,
      target_dir = target_dir,
      add_session_subdir = add_session_subdir,
      overwrite = overwrite,
      make_portable_fn = make_portable_fn,
      timeout_secs = timeout_secs,
      vb = vb,
      rq = rq,
      .progress = vb
    ) |>
      purrr::list_c()
  }
