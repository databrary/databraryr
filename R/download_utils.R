# Internal helpers for the Django signed-download workflow.

#' @noRd
request_processing_task <- function(path, rq = NULL, vb = FALSE) {
  task <- perform_api_get(
    path = path,
    rq = rq,
    vb = vb,
    normalize = TRUE
  )

  if (is.null(task)) {
    if (vb) {
      message("Cannot access requested resource on Databrary. Exiting.")
    }
    return(NULL)
  }

  if (vb && !is.null(task$message)) {
    message(task$message)
  }

  class(task) <- unique(c("databrary_processing_task", class(task)))
  task
}

#' @noRd
request_signed_download_link <- function(path, rq = NULL, vb = FALSE) {
  link <- perform_api_get(
    path = path,
    rq = rq,
    vb = vb,
    normalize = TRUE
  )

  if (is.null(link)) {
    if (vb) {
      message("Cannot access requested resource on Databrary. Exiting.")
    }
    return(NULL)
  }

  if (is.null(link$download_url)) {
    if (vb) {
      message("Download link payload missing 'download_url'.")
    }
    return(NULL)
  }

  link$download_url <- ensure_absolute_url(link$download_url)
  class(link) <- unique(c("databrary_signed_download", class(link)))
  link
}

#' @noRd
ensure_absolute_url <- function(url) {
  assertthat::assert_that(assertthat::is.string(url))
  if (startsWith(url, "http://") || startsWith(url, "https://")) {
    return(url)
  }
  paste0(DATABRARY_BASE_URL, ensure_leading_slash(url))
}

#' @noRd
download_signed_file <- function(download_url,
                                 dest_path,
                                 timeout_secs = REQUEST_TIMEOUT_VERY_LONG,
                                 vb = FALSE) {
  assertthat::assert_that(assertthat::is.string(download_url))
  assertthat::assert_that(assertthat::is.string(dest_path))
  assertthat::is.number(timeout_secs)
  assertthat::assert_that(timeout_secs > 0)

  parent_dir <- dirname(dest_path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }
  assertthat::is.writeable(parent_dir)

  req <- httr2::request(download_url) |
    httr2::req_timeout(seconds = timeout_secs)

  if (vb) {
    message("Saving download to '", dest_path, "'.")
  }

  tryCatch(
    {
      httr2::req_perform(req, path = dest_path)
      dest_path
    },
    httr2_error = function(cnd) {
      if (vb) {
        message("Download failed: ", conditionMessage(cnd))
      }
      NULL
    }
  )
}


