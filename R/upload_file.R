#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Guess the MIME type of a local file from its extension.
#' @noRd
guess_content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!nzchar(ext)) {
    return("application/octet-stream")
  }
  mimes <- list(
    mp4 = "video/mp4", mov = "video/quicktime", avi = "video/x-msvideo",
    mkv = "video/x-matroska", webm = "video/webm",
    mp3 = "audio/mpeg", wav = "audio/wav", m4a = "audio/mp4",
    jpg = "image/jpeg", jpeg = "image/jpeg", png = "image/png",
    gif = "image/gif", tiff = "image/tiff",
    pdf = "application/pdf", csv = "text/csv",
    txt = "text/plain", json = "application/json",
    zip = "application/zip"
  )
  mt <- mimes[[ext]]
  if (is.null(mt)) "application/octet-stream" else mt
}

#' PUT each part of a multipart upload and collect ETags.
#' @noRd
put_multipart_parts <- function(path, part_urls, part_size, file_size, vb) {
  parts_meta <- normalize_part_urls(part_urls)
  results <- vector("list", length(parts_meta))

  for (i in seq_along(parts_meta)) {
    pn <- as.integer(parts_meta[[i]]$part_number)
    url <- parts_meta[[i]]$url
    offset <- (pn - 1L) * as.numeric(part_size)
    remaining <- file_size - offset
    n <- min(as.numeric(part_size), remaining)

    if (vb) {
      message(sprintf(
        "Uploading part %d/%d (%.2f MB)",
        pn, length(parts_meta), n / 1024 / 1024
      ))
    }

    bytes <- read_file_chunk(path, offset = offset, n = n)
    resp <- put_signed_url(url, bytes, vb = vb)
    if (is.null(resp)) {
      if (vb) message("Aborting: part ", pn, " failed to upload.")
      return(NULL)
    }

    etag <- httr2::resp_header(resp, "ETag")
    if (is.null(etag) || !nzchar(etag)) {
      if (vb) message("Aborting: part ", pn, " returned no ETag.")
      return(NULL)
    }
    results[[i]] <- list(part_number = pn, etag = unquote_etag(etag))
  }

  results
}

#' Upload a Local File to Databrary
#'
#' @description High-level wrapper around the upload pipeline:
#' \code{\link{initiate_upload}} -> PUT bytes to the signed URL(s) ->
#' \code{\link{complete_upload}} (multipart only). Returns the metadata
#' needed to poll progress with \code{\link{get_upload_status}}.
#'
#' Picks the upload mode the server returned: single PUT on the on-prem
#' core deployment or for small files on AWS, S3 multipart for large files
#' on AWS.
#'
#' @param path Path to the local file. Must exist and be readable.
#' @param destination_type See \code{\link{initiate_upload}}.
#' @param object_id See \code{\link{initiate_upload}}.
#' @param filename Optional display filename; defaults to \code{basename(path)}.
#' @param content_type Optional MIME type; auto-detected from the file
#'   extension when not supplied.
#' @param source_session_id See \code{\link{initiate_upload}}.
#' @param source_folder_id See \code{\link{initiate_upload}}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A named list with \code{upload_guid} (when known),
#'   \code{status_url}, and \code{upload_type}. Returns \code{NULL} if any
#'   step fails.
#'
#' @inheritParams options_params
#'
#' @seealso \code{\link{initiate_upload}}, \code{\link{get_upload_status}},
#'   \code{\link{complete_upload}}
#'
#' @examples
#' \donttest{
#' \dontrun{
#' info <- upload_file(
#'   path = "/tmp/clip.mp4",
#'   destination_type = "session",
#'   object_id = 42
#' )
#' get_upload_status(status_url = info$status_url)
#' }
#' }
#' @export
upload_file <- function(
  path,
  destination_type,
  object_id,
  filename = NULL,
  content_type = NULL,
  source_session_id = NULL,
  source_folder_id = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assertthat::assert_that(
    is.character(path), length(path) == 1, nzchar(path),
    file.exists(path),
    msg = "path must point to an existing local file"
  )
  if (is.null(filename)) {
    filename <- basename(path)
  }
  if (is.null(content_type)) {
    content_type <- guess_content_type(path)
  }
  file_size <- file.info(path)$size
  assertthat::assert_that(
    !is.na(file_size), file_size > 0,
    msg = paste("file is empty or unreadable:", path)
  )

  init <- initiate_upload(
    filename = filename,
    destination_type = destination_type,
    object_id = object_id,
    file_size = file_size,
    content_type = content_type,
    source_session_id = source_session_id,
    source_folder_id = source_folder_id,
    vb = vb,
    rq = rq
  )
  if (is.null(init)) {
    return(NULL)
  }

  if (identical(init$upload_type, "multipart")) {
    parts <- put_multipart_parts(
      path = path,
      part_urls = init$part_urls,
      part_size = init$part_size,
      file_size = file_size,
      vb = vb
    )
    if (is.null(parts)) {
      return(NULL)
    }

    ok <- complete_upload(
      upload_guid = init$upload_guid,
      s3_upload_id = init$s3_upload_id,
      parts = parts,
      vb = vb,
      rq = rq
    )
    if (is.null(ok)) {
      return(NULL)
    }

    return(list(
      upload_guid = init$upload_guid,
      status_url = init$status_url,
      upload_type = "multipart"
    ))
  }

  # Single PUT path -- AWS deployment returns required_headers (e.g. KMS
  # SSE headers); on-prem core returns no header map.
  headers <- init$required_headers
  if (is.null(headers)) headers <- list()
  # required_headers may arrive as a named list; httr2::req_headers wants a
  # named character vector or `...` so coerce.
  if (is.list(headers) && length(headers) > 0) {
    headers <- vapply(headers, as.character, character(1))
  }

  bytes <- readBin(path, what = "raw", n = file_size)
  resp <- put_signed_url(
    url = init$signed_upload_url,
    body = bytes,
    headers = headers,
    vb = vb
  )
  if (is.null(resp)) {
    return(NULL)
  }

  list(
    upload_guid = init$upload_guid,
    status_url = init$status_url,
    upload_type = "single"
  )
}
