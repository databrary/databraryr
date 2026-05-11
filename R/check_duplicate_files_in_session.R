#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Check Whether Filenames Already Exist in a Session
#'
#' @description Ask the server which of the supplied filenames already
#' exist as files in the given session. Useful before bulk uploads to detect
#' name collisions in advance.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param filenames Character vector of filenames to check. Length must be
#'   at least 1; each element must be a non-empty string.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return A \code{tibble} with columns \code{filename} (character) and
#'   \code{exists} (logical), one row per input filename and in the same
#'   order. Returns \code{NULL} if the request fails.
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' check_duplicate_files_in_session(
#'   vol_id = 1,
#'   session_id = 42,
#'   filenames = c("clip_001.mp4", "clip_002.mp4")
#' )
#' }
#' }
#' @export
check_duplicate_files_in_session <- function( # nolint: object_length_linter.
  vol_id = 1,
  session_id,
  filenames,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")

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
    path = sprintf(API_SESSION_CHECK_DUPLICATE_FILES, vol_id, session_id),
    body = body,
    rq = rq,
    vb = vb
  )

  if (is.null(result) || isTRUE(result)) {
    if (vb) {
      message(
        "Failed to check duplicate filenames in session ",
        session_id, " of volume ", vol_id
      )
    }
    return(NULL)
  }

  tibble::tibble(
    filename = vapply(result, function(r) as.character(r$filename), character(1)),
    exists = vapply(result, function(r) isTRUE(r$exists), logical(1))
  )
}
