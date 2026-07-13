#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Bulk Create Records in a Databrary Volume
#'
#' @description Create many records sequentially via
#' \code{\link{create_volume_record}} with empty \code{measures} aside from the
#' resolved name metric. \code{record_names} must be unique (for
#' \code{\link{resume_bulk}}). \code{category_id} may be length 1 or match
#' \code{record_names}. Per-row \code{measures} / \code{participant} are not
#' supported; call \code{\link{create_volume_record}} for those cases.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param record_names Non-empty character vector of record display names (trimmed).
#' @param category_id Numeric category id(s); length 1 or \code{length(record_names)}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}};
#'   \code{input} is each trimmed record name.
#'
#' @seealso \code{\link{create_volume_record}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_create_records(
#'   vol_id = 1,
#'   record_names = c("P101", "P102"),
#'   category_id = 6
#' )
#' }
#' }
#' @export
bulk_create_records <- function(
  vol_id = 1,
  record_names,
  category_id,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  trimmed <- assert_bulk_names(record_names, "record_names")
  n <- length(trimmed)
  assertthat::assert_that(
    !anyDuplicated(trimmed),
    msg = "record_names must be unique for bulk_create_records() (required for resume_bulk)"
  )
  if (length(category_id) == 1L) {
    assert_positive_integer(category_id, "category_id")
  } else {
    assert_positive_integer_vec(category_id, "category_id")
  }
  assert_recyclable(category_id, n, "category_id")

  assert_positive_integer(vol_id, "vol_id")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = trimmed,
    fn = function(nm) {
      i <- match(nm, trimmed)
      create_volume_record(
        vol_id = vol_id,
        category_id = recycle_i(category_id, i),
        name = nm,
        measures = list(),
        participant = NULL,
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

#' Bulk Delete Records from a Databrary Volume
#'
#' @description Soft-delete many volume records sequentially via
#' \code{\link{delete_volume_record}}.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param record_ids Numeric vector of record identifiers (positive integers).
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}};
#'   \code{input} is each record id.
#'
#' @seealso \code{\link{delete_volume_record}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_delete_records(vol_id = 1, record_ids = c(101, 102))
#' }
#' }
#' @export
bulk_delete_records <- function(
  vol_id = 1,
  record_ids,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer_vec(record_ids, "record_ids")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = record_ids,
    fn = function(id) {
      delete_volume_record(vol_id = vol_id, record_id = id, vb = vb, rq = rq)
    },
    is_failure = function(res) is.null(res) || isFALSE(res),
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}

#' Bulk Assign Default Records to a Session
#'
#' @description For each record id, call \code{\link{add_default_record_to_session}}
#' so the record becomes a session default.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier.
#' @param record_ids Numeric vector of record identifiers.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}};
#'   \code{input} is each record id.
#'
#' @seealso \code{\link{add_default_record_to_session}},
#'   \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_assign_records(vol_id = 1, session_id = 42, record_ids = c(101, 102))
#' }
#' }
#' @export
bulk_assign_records <- function(
  vol_id = 1,
  session_id,
  record_ids,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer_vec(record_ids, "record_ids")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = record_ids,
    fn = function(id) {
      add_default_record_to_session(
        vol_id = vol_id,
        session_id = session_id,
        record_id = id,
        vb = vb,
        rq = rq
      )
    },
    is_failure = function(res) isFALSE(res),
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}

#' Bulk Unassign Default Records from a Session
#'
#' @description For each record id, call
#' \code{\link{remove_default_record_from_session}}.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param session_id Numeric session identifier.
#' @param record_ids Numeric vector of record identifiers.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}};
#'   \code{input} is each record id.
#'
#' @seealso \code{\link{remove_default_record_from_session}},
#'   \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_unassign_records(vol_id = 1, session_id = 42, record_ids = c(101, 102))
#' }
#' }
#' @export
bulk_unassign_records <- function(
  vol_id = 1,
  session_id,
  record_ids,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer(session_id, "session_id")
  assert_positive_integer_vec(record_ids, "record_ids")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = record_ids,
    fn = function(id) {
      remove_default_record_from_session(
        vol_id = vol_id,
        session_id = session_id,
        record_id = id,
        vb = vb,
        rq = rq
      )
    },
    is_failure = function(res) isFALSE(res),
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}
