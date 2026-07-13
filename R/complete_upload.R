#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Complete a Multipart Upload
#'
#' @description Tell the Databrary API that all parts of a multipart upload
#' have been PUT to S3 and the object can be assembled. The server forwards
#' the part list to S3's \code{CompleteMultipartUpload}, transitioning the
#' upload into the post-upload pipeline (virus scan, format probe).
#'
#' Only needed when \code{initiate_upload()} returned
#' \code{upload_type == "multipart"}. Single-PUT uploads complete implicitly
#' once the object lands in storage.
#'
#' @param upload_guid Character GUID returned in \code{initiate_upload()$upload_guid}.
#' @param s3_upload_id Character S3 upload ID returned in
#'   \code{initiate_upload()$s3_upload_id}.
#' @param parts A list of one entry per part, each a list with
#'   \code{part_number} (positive integer) and \code{etag} (character,
#'   from the \code{ETag} response header of the part PUT). Order is not
#'   significant -- the server sorts by \code{part_number}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} on success, \code{NULL} on failure.
#'
#' @inheritParams options_params
#'
#' @seealso \code{\link{initiate_upload}}, \code{\link{upload_file}}
#'
#' @examples
#' \donttest{
#' \dontrun{
#' complete_upload(
#'   upload_guid = "abc...",
#'   s3_upload_id = "xyz...",
#'   parts = list(
#'     list(part_number = 1L, etag = "etag-of-part-1"),
#'     list(part_number = 2L, etag = "etag-of-part-2")
#'   )
#' )
#' }
#' }
#' @export
complete_upload <- function(
  upload_guid,
  s3_upload_id,
  parts,
  vb = options::opt("vb"),
  rq = NULL
) {
  assertthat::assert_that(
    is.character(upload_guid), length(upload_guid) == 1, nzchar(upload_guid)
  )
  assertthat::assert_that(
    is.character(s3_upload_id), length(s3_upload_id) == 1, nzchar(s3_upload_id)
  )
  assertthat::assert_that(
    is.list(parts), length(parts) >= 1,
    msg = "parts must be a non-empty list"
  )

  # Coerce each entry to the wire shape and validate. Mutates only the local
  # copy.
  parts <- lapply(parts, function(p) {
    assertthat::assert_that(
      is.list(p), !is.null(p$part_number), !is.null(p$etag),
      msg = "each parts entry must be a list with part_number and etag"
    )
    pn <- p$part_number
    assertthat::assert_that(
      is.numeric(pn), length(pn) == 1, pn >= 1, pn == floor(pn),
      msg = "part_number must be a positive integer"
    )
    assertthat::assert_that(
      is.character(p$etag), length(p$etag) == 1, nzchar(p$etag),
      msg = "etag must be a non-empty string"
    )
    list(part_number = as.integer(pn), etag = unquote_etag(p$etag))
  })

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  result <- perform_api_post(
    path = API_UPLOADS_COMPLETE_MULTIPART,
    body = list(
      upload_guid = upload_guid,
      s3_upload_id = s3_upload_id,
      parts = parts
    ),
    rq = rq,
    vb = vb
  )

  if (is.null(result)) {
    if (vb) {
      message("Failed to complete multipart upload ", upload_guid)
    }
    return(NULL)
  }
  TRUE
}
