#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Initiate a File Upload
#'
#' @description Ask the Databrary API to register a pending upload and return
#' the signed URL(s) needed to PUT file bytes directly to object storage. The
#' server decides whether to issue a single-PUT or a multipart upload based on
#' \code{file_size} and the deployment's storage backend (S3 multipart on the
#' AI/AWS deployment, single PUT on the on-prem core deployment).
#'
#' This is the first step of the upload pipeline; pass the returned object to
#' \code{\link{upload_file}} (high level) or use the returned URLs directly
#' with \code{httr2::req_perform()} for fine-grained control.
#'
#' @param filename Display filename. Required, non-empty.
#' @param destination_type Where the upload will live. One of
#'   \code{"session"}, \code{"folder"}, \code{"linked_volume_session"},
#'   \code{"linked_volume_folder"} (server validates). Required.
#' @param object_id Positive integer ID of the destination object (session
#'   or folder, depending on \code{destination_type}). Required.
#' @param file_size File size in bytes. Optional but strongly recommended:
#'   the AWS deployment uses this to decide multipart vs single PUT.
#' @param content_type MIME type of the file (e.g. \code{"video/mp4"}).
#'   Optional but recommended; some storage backends require it.
#' @param source_session_id Required when
#'   \code{destination_type == "linked_volume_session"}; positive integer.
#' @param source_folder_id Required when
#'   \code{destination_type == "linked_volume_folder"}; positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A named list describing the upload. Always contains
#'   \code{status_url}. For a single PUT (the on-prem core deployment, or
#'   any AWS upload smaller than the multipart threshold), contains
#'   \code{signed_upload_url} and \code{required_headers} (may be empty).
#'   For a multipart upload, contains \code{upload_type = "multipart"},
#'   \code{upload_guid}, \code{s3_upload_id}, \code{part_urls} (a list of
#'   \code{list(part_number, url)}), and \code{part_size}. Returns
#'   \code{NULL} if the request fails.
#'
#' @inheritParams options_params
#'
#' @seealso \code{\link{upload_file}}, \code{\link{get_upload_status}},
#'   \code{\link{complete_upload}}
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Initiate a small upload to a session
#' init <- initiate_upload(
#'   filename = "clip_001.mp4",
#'   destination_type = "session",
#'   object_id = 42,
#'   file_size = 1024L * 1024L,
#'   content_type = "video/mp4"
#' )
#' }
#' }
#' @export
initiate_upload <- function(
  filename,
  destination_type,
  object_id,
  file_size = NULL,
  content_type = NULL,
  source_session_id = NULL,
  source_folder_id = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assertthat::assert_that(
    is.character(filename), length(filename) == 1, nzchar(trimws(filename)),
    msg = "filename must be a non-empty string"
  )
  assertthat::assert_that(
    is.character(destination_type), length(destination_type) == 1,
    nzchar(trimws(destination_type)),
    msg = "destination_type must be a non-empty string"
  )
  assert_positive_integer(object_id, "object_id")

  if (!is.null(file_size)) {
    assert_positive_integer(file_size, "file_size")
  }
  if (!is.null(content_type)) {
    assertthat::assert_that(
      is.character(content_type), length(content_type) == 1,
      nzchar(trimws(content_type))
    )
  }
  if (!is.null(source_session_id)) {
    assert_positive_integer(source_session_id, "source_session_id")
  }
  if (!is.null(source_folder_id)) {
    assert_positive_integer(source_folder_id, "source_folder_id")
  }

  # Server enforces the linked-volume requirements, but fail fast client-side
  # so users get a useful message before the round-trip.
  if (identical(destination_type, "linked_volume_session")) {
    assertthat::assert_that(
      !is.null(source_session_id),
      msg = "source_session_id required when destination_type == 'linked_volume_session'"
    )
  }
  if (identical(destination_type, "linked_volume_folder")) {
    assertthat::assert_that(
      !is.null(source_folder_id),
      msg = "source_folder_id required when destination_type == 'linked_volume_folder'"
    )
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  body <- list(
    filename = filename,
    destination_type = destination_type,
    object_id = as.integer(object_id)
  )
  if (!is.null(file_size)) body$file_size <- as.integer(file_size)
  if (!is.null(content_type)) body$content_type <- content_type
  if (!is.null(source_session_id)) {
    body$source_session_id <- as.integer(source_session_id)
  }
  if (!is.null(source_folder_id)) {
    body$source_folder_id <- as.integer(source_folder_id)
  }

  result <- perform_api_post(
    path = API_UPLOADS_INITIATE,
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(result) || isTRUE(result)) {
    if (vb) {
      message("Failed to initiate upload for filename ", filename)
    }
    return(NULL)
  }

  # The on-prem core deployment omits `upload_type`; default it so callers
  # can branch on a single field without checking presence.
  if (is.null(result$upload_type)) {
    result$upload_type <- "single"
  }
  result
}
