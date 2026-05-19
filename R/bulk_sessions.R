#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Bulk Delete Sessions from a Databrary Volume
#'
#' @description Soft-delete many sessions from a single volume sequentially.
#' With \code{on_error = "stop"} (default), fails fast and throws
#' \code{databraryr_bulk_error}; with \code{"collect"}, marks failed rows and
#' continues.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_ids Numeric vector of session identifiers. Must be positive
#'   integers.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}}.
#'
#' @seealso \code{\link{delete_session}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_delete_sessions(vol_id = 1, session_ids = c(101, 102, 103))
#' }
#' }
#' @export
bulk_delete_sessions <- function(
  vol_id = 1,
  session_ids,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer_vec(session_ids, "session_ids")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = session_ids,
    fn = function(id) {
      delete_session(vol_id = vol_id, session_id = id, vb = vb, rq = rq)
    },
    is_failure = function(res) is.null(res) || isFALSE(res),
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}

#' Bulk Rename Sessions in a Databrary Volume
#'
#' @description Rename many sessions sequentially via \code{\link{patch_session}}
#' (\code{name} only). \code{session_ids} must be unique so each row maps to one
#' name. With \code{\link{resume_bulk}}, pass \code{new_names} aligned to the
#' subset of ids being retried (same order as incomplete rows).
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_ids Numeric vector of session identifiers (unique positive
#'   integers).
#' @param new_names Character vector of new names, same length as
#'   \code{session_ids}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}};
#'   \code{input} is each session id.
#'
#' @seealso \code{\link{patch_session}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_rename_sessions(
#'   vol_id = 1,
#'   session_ids = c(101, 102),
#'   new_names = c("Lab visit A", "Lab visit B")
#' )
#' }
#' }
#' @export
bulk_rename_sessions <- function(
  vol_id = 1,
  session_ids,
  new_names,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer_vec(session_ids, "session_ids")
  trimmed_names <- assert_bulk_names(new_names, "new_names")
  assertthat::assert_that(
    length(trimmed_names) == length(session_ids),
    msg = "new_names must have the same length as session_ids"
  )
  assertthat::assert_that(
    !anyDuplicated(session_ids),
    msg = "session_ids must be unique for bulk_rename_sessions()"
  )
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = session_ids,
    fn = function(id) {
      i <- match(id, session_ids)
      patch_session(
        vol_id = vol_id,
        session_id = id,
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

#' Bulk Create Sessions in a Databrary Volume
#'
#' @description Create many sessions sequentially. \code{input} in the result
#' tibble is the session name for that row (after trimming). \code{session_names}
#' must be unique. Optional fields are recycled. Structured \code{date},
#' \code{date_precision}, and
#' \code{default_records} are not supported here; use \code{\link{create_session}}
#' per row if you need them.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_names Non-empty character vector of session names.
#' @param release_level Optional character vector (length 1 or
#'   \code{length(session_names)}).
#' @param source_date Optional \code{Date}, ISO string, or vector thereof
#'   (length 1 or \code{length(session_names)}).
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}},
#'   with \code{input} equal to the trimmed session name for each row.
#'
#' @seealso \code{\link{create_session}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_create_sessions(vol_id = 1, session_names = c("S1", "S2"))
#' }
#' }
#' @export
bulk_create_sessions <- function(
  vol_id = 1,
  session_names,
  release_level = NULL,
  source_date = NULL,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  trimmed <- assert_bulk_names(session_names, "session_names")
  n <- length(trimmed)
  assertthat::assert_that(
    !anyDuplicated(trimmed),
    msg = "session_names must be unique for bulk_create_sessions() (required for resume_bulk)"
  )
  assert_recyclable(release_level, n, "release_level")
  assert_recyclable(source_date, n, "source_date")

  assert_positive_integer(vol_id, "vol_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = trimmed,
    fn = function(nm) {
      i <- match(nm, trimmed)
      create_session(
        vol_id = vol_id,
        name = nm,
        release_level = recycle_i(release_level, i),
        source_date = recycle_i(source_date, i),
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
