#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Delete a Session File from a Databrary Volume
#'
#' @description Delete (soft-delete) a file from a Databrary session. The file
#' is marked as deleted on the server but not permanently removed. A second
#' delete on the same file returns \code{FALSE}, not an error.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param file_id Numeric file identifier. Must be a positive integer.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#'
#' @return \code{TRUE} if the file was successfully deleted, \code{FALSE}
#'   otherwise.
#'
#' @seealso \code{\link{update_session_file}}, \code{\link{patch_session_file}},
#'   \code{\link{get_session_file}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' delete_session_file(vol_id = 1, session_id = 42, file_id = 99)
#' }
#' }
#' @export
delete_session_file <- function(
  vol_id = 1,
  session_id,
  file_id,
  vb = options::opt("vb"),
  rq = NULL
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer(file_id, "file_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  success <- perform_api_delete(
    path = sprintf(API_SESSION_FILE_DETAIL, vol_id, session_id, file_id),
    rq = rq,
    vb = vb
  )

  if (!success) {
    if (vb) {
      message(
        "Failed to delete file ", file_id,
        " from session ", session_id,
        " in volume ", vol_id
      )
    }
  }

  success
}
