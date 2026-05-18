#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Bulk Upload Files to a Databrary Session or Folder
#'
#' @description Upload many files sequentially to a single session or a single
#' folder. Optionally runs a preflight check for duplicate basenames and skips
#' those uploads. With \code{on_error = "stop"} (default), fails fast on the
#' first error and throws \code{databraryr_bulk_error} with a partial tibble for
#' \code{\link{resume_bulk}}.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Session id when uploading to a session. Exactly one of
#'   \code{session_id} and \code{folder_id} must be non-\code{NULL}.
#' @param file_paths Character vector of local file paths to upload. Each must
#'   point to an existing file.
#' @param folder_id Folder id when uploading to a folder. Exactly one of
#'   \code{session_id} and \code{folder_id} must be non-\code{NULL}.
#' @param preflight Logical; if \code{TRUE} (default), skip files whose basename
#'   already exists in the target session or folder (via
#'   \code{\link{check_duplicate_files_in_session}} or
#'   \code{\link{check_duplicate_files_in_folder}}).
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} (default) throws \code{databraryr_bulk_error}
#'   on first failure after retries; \code{"collect"} marks failed rows and
#'   continues.
#' @param max_retries Non-negative integer: extra attempts per input after the
#'   first failure (default \code{0}).
#' @param retry_delay Seconds to sleep between retries (default \code{0}).
#'
#' @return A \code{tibble} with one row per input file and columns:
#'   \code{input} (path), \code{status} (\code{"success"}, \code{"failed"},
#'   \code{"skipped"}, or \code{"pending"}), \code{result} (list-column with
#'   the per-file API response or \code{NULL}), \code{error} (character
#'   message on failure), and \code{reason} (e.g. \code{"duplicate"}).
#'
#' @seealso \code{\link{upload_file}},
#'   \code{\link{check_duplicate_files_in_session}},
#'   \code{\link{check_duplicate_files_in_folder}},
#'   \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' result <- bulk_upload_files(
#'   vol_id = 1,
#'   session_id = 42,
#'   file_paths = c("/tmp/a.mp4", "/tmp/b.mp4")
#' )
#' result <- bulk_upload_files(
#'   vol_id = 1,
#'   file_paths = c("/tmp/a.mp4", "/tmp/b.mp4"),
#'   folder_id = 7
#' )
#' }
#' }
#' @export
bulk_upload_files <- function(
  vol_id = 1,
  session_id = NULL,
  file_paths,
  folder_id = NULL,
  preflight = TRUE,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  has_session <- !is.null(session_id)
  has_folder <- !is.null(folder_id)
  assertthat::assert_that(
    xor(has_session, has_folder),
    msg = "Exactly one of session_id and folder_id must be non-NULL"
  )
  if (has_session) {
    assert_positive_integer(session_id, "session_id")
  } else {
    assert_positive_integer(folder_id, "folder_id")
  }
  assert_file_paths(file_paths)
  assertthat::assert_that(is.logical(preflight), length(preflight) == 1)
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  dest_type <- if (has_session) "session" else "folder"
  dest_id <- if (has_session) session_id else folder_id

  preflight_fn <- if (preflight) {
    if (has_session) {
      function(state) {
        preflight_session_duplicates(state, vol_id, session_id, vb, rq)
      }
    } else {
      function(state) {
        preflight_folder_duplicates(state, vol_id, folder_id, vb, rq)
      }
    }
  } else {
    NULL
  }

  bulk_apply(
    inputs = file_paths,
    fn = function(p) {
      upload_file(
        path = p,
        destination_type = dest_type,
        object_id = dest_id,
        vb = vb,
        rq = rq
      )
    },
    is_failure = function(res) is.null(res),
    preflight = preflight_fn,
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}

#' Bulk Delete Files from a Databrary Session
#'
#' @description Soft-delete many files from a single session sequentially.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier. Must be a positive integer.
#' @param file_ids Numeric vector of file identifiers. Must be positive
#'   integers.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}}.
#'
#' @seealso \code{\link{delete_session_file}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_delete_files(vol_id = 1, session_id = 42, file_ids = c(1001, 1002))
#' }
#' }
#' @export
bulk_delete_files <- function(
  vol_id = 1,
  session_id,
  file_ids,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer_vec(file_ids, "file_ids")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = file_ids,
    fn = function(id) {
      delete_session_file(
        vol_id = vol_id, session_id = session_id, file_id = id,
        vb = vb, rq = rq
      )
    },
    is_failure = function(res) is.null(res) || isFALSE(res),
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}

#' Bulk Rename Files in a Databrary Session
#'
#' @description Rename many session files sequentially via
#' \code{\link{patch_session_file}} (\code{name} only). \code{file_ids} must be
#' unique. Folder assets are not supported here (no folder file PATCH helper).
#' With \code{\link{resume_bulk}}, pass \code{new_names} aligned to incomplete
#' rows.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier containing the files.
#' @param file_ids Numeric vector of file identifiers (unique positive integers).
#' @param new_names Character vector of new names, same length as \code{file_ids}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}};
#'   \code{input} is each file id.
#'
#' @seealso \code{\link{patch_session_file}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_rename_files(
#'   vol_id = 1,
#'   session_id = 42,
#'   file_ids = c(1001, 1002),
#'   new_names = c("clip_a.mp4", "clip_b.mp4")
#' )
#' }
#' }
#' @export
bulk_rename_files <- function(
  vol_id = 1,
  session_id,
  file_ids,
  new_names,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer_vec(file_ids, "file_ids")
  trimmed_names <- assert_bulk_names(new_names, "new_names")
  assertthat::assert_that(
    length(trimmed_names) == length(file_ids),
    msg = "new_names must have the same length as file_ids"
  )
  assertthat::assert_that(
    !anyDuplicated(file_ids),
    msg = "file_ids must be unique for bulk_rename_files()"
  )
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = file_ids,
    fn = function(id) {
      i <- match(id, file_ids)
      patch_session_file(
        vol_id = vol_id,
        session_id = session_id,
        file_id = id,
        name = trimmed_names[[i]],
        vb = vb,
        rq = rq
      )
    },
    is_failure = function(res) is.null(res),
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}
