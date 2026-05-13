#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Get the Current Status of an Upload
#'
#' @description Poll the Databrary API for the lifecycle state of an upload
#' started with \code{\link{initiate_upload}}. The returned status reflects
#' both the storage-side transfer and the post-upload server pipeline
#' (e.g. virus scan, format probe).
#'
#' Accepts either the absolute \code{status_url} returned by
#' \code{initiate_upload()} (preferred -- avoids a second URL build) or
#' the upload's \code{upload_guid}.
#'
#' @param status_url Absolute URL returned by \code{initiate_upload()} under
#'   \code{status_url}. Mutually exclusive with \code{upload_guid}.
#' @param upload_guid Character upload GUID. Mutually exclusive with
#'   \code{status_url}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A length-1 character string with the status (e.g.
#'   \code{"scanning"}, \code{"completed"}, \code{"infected"},
#'   \code{"upload_failed"}, \code{"processing_failed"} on the AWS
#'   deployment; raw \code{Upload.status} on the core deployment). Returns
#'   \code{NULL} if the request fails.
#'
#' @inheritParams options_params
#'
#' @seealso \code{\link{initiate_upload}}, \code{\link{upload_file}}
#'
#' @examples
#' \donttest{
#' \dontrun{
#' init <- initiate_upload(
#'   filename = "clip.mp4",
#'   destination_type = "session",
#'   object_id = 42
#' )
#' get_upload_status(status_url = init$status_url)
#' }
#' }
#' @export
get_upload_status <- function(
  status_url = NULL,
  upload_guid = NULL,
  vb = options::opt("vb"),
  rq = NULL
) {
  assertthat::assert_that(
    xor(is.null(status_url), is.null(upload_guid)),
    msg = "Provide exactly one of status_url or upload_guid"
  )

  if (!is.null(status_url)) {
    assertthat::assert_that(
      is.character(status_url), length(status_url) == 1, nzchar(status_url)
    )
    path <- status_url
    absolute <- grepl("^https?://", status_url)
  } else {
    assertthat::assert_that(
      is.character(upload_guid), length(upload_guid) == 1, nzchar(upload_guid)
    )
    path <- sprintf(API_UPLOADS_STATUS, upload_guid)
    absolute <- FALSE
  }

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # perform_api_get prepends DATABRARY_BASE_URL via ensure_leading_slash, so
  # for absolute URLs we override the URL on the request after construction.
  if (absolute) {
    request <- rq
    if (is.null(request)) {
      request <- databraryr::make_default_request()
    }
    request <- httr2::req_url(request, path)

    response <- tryCatch(
      httr2::req_perform(request),
      httr2_error = function(cnd) {
        if (vb) {
          message("Status request failed for ", path, ": ", conditionMessage(cnd))
        }
        NULL
      }
    )
    if (is.null(response)) {
      return(NULL)
    }
    body <- httr2::resp_body_json(response)
  } else {
    body <- perform_api_get(path = path, rq = rq, vb = vb)
  }

  if (is.null(body) || is.null(body$status)) {
    return(NULL)
  }
  as.character(body$status)
}
