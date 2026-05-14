#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Check Whether Filenames Already Exist in a Folder
#'
#' @description Ask the server which of the supplied filenames already
#' exist as files in the given folder. Useful before uploading multiple files to detect
#' name collisions in advance.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param folder_id Numeric folder identifier. Must be a positive integer.
#' @param filenames Character vector of filenames to check. Length must be
#'   at least 1; each element must be a non-empty string.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A \code{tibble} with columns \code{filename} (character) and
#'   \code{exists} (logical), one row per input filename and in the same
#'   order. Returns \code{NULL} if the request fails. Against current staging API,
#'   a missing \code{folder_id} still yields a successful response with
#'   \code{exists = FALSE} for each filename (nothing matches that folder).
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' check_duplicate_files_in_folder(
#'   vol_id = 1,
#'   folder_id = 42,
#'   filenames = c("clip_001.mp4", "clip_002.mp4")
#' )
#' }
#' }
#' @export
check_duplicate_files_in_folder <- function( # nolint: object_length_linter.
  vol_id = 1,
  folder_id,
  filenames,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(folder_id, "folder_id")

  assertthat::assert_that(
    is.character(filenames),
    length(filenames) >= 1,
    msg = "filenames must be a non-empty character vector"
  )
  assertthat::assert_that(
    !any(is.na(filenames)),
    all(nzchar(trimws(filenames))),
    msg = "filenames must not contain NA or empty strings"
  )

  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  # `as.list()` on a length-1 character would still serialize to a JSON array
  # via httr2, but be explicit so the contract matches the server's expected
  # `[...]` shape regardless of length.
  body <- list(filenames = as.list(filenames))

  result <- perform_api_post(
    path = sprintf(API_FOLDER_CHECK_DUPLICATE_FILES, vol_id, folder_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(result) || isTRUE(result)) {
    if (vb) {
      message(
        "Failed to check duplicate filenames in folder ",
        folder_id, " of volume ", vol_id
      )
    }
    return(NULL)
  }

  tibble::tibble(
    filename = vapply(result, function(r) as.character(r$filename), character(1)),
    exists = vapply(result, function(r) isTRUE(r$exists), logical(1))
  )
}
