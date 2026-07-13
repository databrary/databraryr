# Internal helpers for the file upload pipeline (initiate / PUT / complete).

#' Read a byte range from a file as a raw vector.
#'
#' Used to slice a part for multipart upload. Reads at most `n` bytes
#' starting at `offset`; the last part may return fewer bytes than `n`.
#' @noRd
read_file_chunk <- function(path, offset, n) {
  assertthat::assert_that(
    assertthat::is.string(path),
    file.exists(path)
  )
  assertthat::assert_that(
    is.numeric(offset), length(offset) == 1, offset >= 0
  )
  assertthat::assert_that(
    is.numeric(n), length(n) == 1, n >= 0
  )

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  if (offset > 0) {
    seek(con, where = offset, origin = "start")
  }
  readBin(con, what = "raw", n = n)
}

#' Strip surrounding quotes from an S3 ETag header value.
#'
#' S3 returns the ETag wrapped in double quotes (e.g. `"abc123"`); the
#' completion endpoint accepts either form, but stripping keeps the value
#' tidy and matches the boto example in the service layer.
#' @noRd
unquote_etag <- function(etag) {
  if (is.null(etag) || !nzchar(etag)) {
    return(etag)
  }
  gsub('^"|"$', "", etag)
}

#' PUT a raw byte payload to a signed URL.
#'
#' Builds a *fresh* httr2 request (no Databrary auth headers) because the
#' presigned URL embeds its own signature; sending extra Authorization
#' headers would invalidate it on both S3 and GCS.
#'
#' @param url Signed upload URL.
#' @param body Raw vector to upload.
#' @param headers Optional named character vector of headers (e.g. the
#'   `required_headers` returned by `initiate_upload()` for single PUTs).
#' @param vb Verbose logging flag.
#' @return The httr2 response object on success, `NULL` on failure.
#' @noRd
put_signed_url <- function(url, body, headers = NULL, vb = FALSE) {
  assertthat::assert_that(assertthat::is.string(url))
  assertthat::assert_that(is.raw(body))

  request <- httr2::request(url)
  request <- httr2::req_method(request, "PUT")
  request <- httr2::req_body_raw(request, body)
  if (!is.null(headers) && length(headers) > 0) {
    request <- do.call(httr2::req_headers, c(list(request), as.list(headers)))
  }
  request <- httr2::req_timeout(request, REQUEST_TIMEOUT_VERY_LONG)
  request <- httr2::req_retry(
    request,
    max_tries = RETRY_LIMIT,
    backoff = function(i) RETRY_WAIT_TIME * RETRY_BACKOFF^(i - 1)
  )

  tryCatch(
    httr2::req_perform(request),
    httr2_error = function(cnd) {
      if (vb) {
        message("PUT failed for signed URL: ", conditionMessage(cnd))
      }
      NULL
    }
  )
}

#' Normalize the `part_urls` element returned by `initiate_upload()`.
#'
#' Each entry is `list(part_number = int, url = string)`. We sort by
#' part_number to be defensive against server ordering.
#' @noRd
normalize_part_urls <- function(part_urls) {
  assertthat::assert_that(
    is.list(part_urls), length(part_urls) >= 1,
    msg = "part_urls must be a non-empty list"
  )
  numbers <- vapply(part_urls, function(p) as.integer(p$part_number), integer(1))
  part_urls[order(numbers)]
}
