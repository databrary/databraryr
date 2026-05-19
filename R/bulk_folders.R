#' @eval options::as_params()
#' @name options_params
#'
NULL

#' Bulk Delete Folders from a Databrary Volume
#'
#' @description Soft-delete many folders from a single volume sequentially.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param folder_ids Numeric vector of folder identifiers. Must be positive
#'   integers.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}}.
#'
#' @seealso \code{\link{delete_folder}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_delete_folders(vol_id = 1, folder_ids = c(11, 12, 13))
#' }
#' }
#' @export
bulk_delete_folders <- function(
  vol_id = 1,
  folder_ids,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer_vec(folder_ids, "folder_ids")
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = folder_ids,
    fn = function(id) {
      delete_folder(vol_id = vol_id, folder_id = id, vb = vb, rq = rq)
    },
    is_failure = function(res) is.null(res) || isFALSE(res),
    on_error = on_error,
    max_retries = max_retries,
    retry_delay = retry_delay
  )
}

#' Bulk Rename Folders in a Databrary Volume
#'
#' @description Rename many folders sequentially via \code{\link{patch_folder}}
#' (\code{name} only). \code{folder_ids} must be unique. With
#' \code{\link{resume_bulk}}, pass \code{new_names} aligned to incomplete rows.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param folder_ids Numeric vector of folder identifiers (unique positive
#'   integers).
#' @param new_names Character vector of new names, same length as
#'   \code{folder_ids}.
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}};
#'   \code{input} is each folder id.
#'
#' @seealso \code{\link{patch_folder}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_rename_folders(
#'   vol_id = 1,
#'   folder_ids = c(11, 12),
#'   new_names = c("Stimuli", "Protocols")
#' )
#' }
#' }
#' @export
bulk_rename_folders <- function(
  vol_id = 1,
  folder_ids,
  new_names,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  assert_positive_integer(vol_id, "vol_id")
  assert_positive_integer_vec(folder_ids, "folder_ids")
  trimmed_names <- assert_bulk_names(new_names, "new_names")
  assertthat::assert_that(
    length(trimmed_names) == length(folder_ids),
    msg = "new_names must have the same length as folder_ids"
  )
  assertthat::assert_that(
    !anyDuplicated(folder_ids),
    msg = "folder_ids must be unique for bulk_rename_folders()"
  )
  assertthat::assert_that(is.logical(vb), length(vb) == 1)
  assertthat::assert_that(is.null(rq) || inherits(rq, "httr2_request"))

  bulk_apply(
    inputs = folder_ids,
    fn = function(id) {
      i <- match(id, folder_ids)
      patch_folder(
        vol_id = vol_id,
        folder_id = id,
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

#' Bulk Create Folders in a Databrary Volume
#'
#' @description Create many folders sequentially. \code{input} in the result
#' tibble is the folder name for that row (after trimming). \code{folder_names}
#' must be unique. Optional fields are recycled: length 1 or same length as
#' \code{folder_names}.
#'
#' @param vol_id Target volume number. Must be a positive integer.
#' @param folder_names Non-empty character vector of folder names (trimmed;
#'   none may be empty after trimming).
#' @param release_level Optional character vector (length 1 or
#'   \code{length(folder_names)}), passed per row to \code{\link{create_folder}}.
#' @param source_date Optional \code{Date}, ISO string, or vector thereof
#'   (length 1 or \code{length(folder_names)}).
#' @param rq An \code{httr2} request object. Defaults to \code{NULL}.
#' @param on_error \code{"stop"} or \code{"collect"}; see \code{\link{bulk_upload_files}}.
#' @param max_retries Non-negative integer; extra attempts per input after the first failure.
#' @param retry_delay Seconds between retries.
#'
#' @return A \code{tibble} as documented in \code{\link{bulk_upload_files}},
#'   with \code{input} equal to the trimmed folder name for each row.
#'
#' @seealso \code{\link{create_folder}}, \code{\link{resume_bulk}}
#'
#' @inheritParams options_params
#'
#' @examples
#' \donttest{
#' \dontrun{
#' bulk_create_folders(vol_id = 1, folder_names = c("A", "B", "C"))
#' }
#' }
#' @export
bulk_create_folders <- function(
  vol_id = 1,
  folder_names,
  release_level = NULL,
  source_date = NULL,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
) {
  trimmed <- assert_bulk_names(folder_names, "folder_names")
  n <- length(trimmed)
  assertthat::assert_that(
    !anyDuplicated(trimmed),
    msg = "folder_names must be unique for bulk_create_folders() (required for resume_bulk)"
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
      create_folder(
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
