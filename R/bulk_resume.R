#' Resume a Bulk Operation After a Failure
#'
#' @description When a \code{bulk_*} run ends with incomplete rows (either
#' because \code{on_error = "stop"} threw \code{databraryr_bulk_error} or
#' because \code{on_error = "collect"} left \code{"failed"} / \code{"pending"}
#' rows), \code{resume_bulk()} re-runs the supplied bulk function on those rows
#' and merges the result, preserving original input order.
#'
#' @param partial A tibble produced by a previous \code{bulk_*} call (typically
#'   extracted from a \code{databraryr_bulk_error} condition via
#'   \code{conditionMessage} / direct access to \code{cond$partial}).
#' @param fn The bulk function to re-invoke (e.g. \code{bulk_upload_files}).
#' @param ... Additional arguments forwarded to \code{fn} (for example
#'   \code{vol_id}, \code{session_id}, \code{vb}, \code{rq}). The remaining
#'   inputs are passed as the function's input vector argument under the name
#'   \code{input_arg} (default: auto-detected from the function's formals).
#' @param input_arg Optional name of the input argument of \code{fn}. If
#'   \code{NULL} (default), it is inferred as the first formal of \code{fn}
#'   matching one of \code{file_paths}, \code{session_ids}, \code{folder_ids},
#'   \code{file_ids}, \code{session_names}, \code{folder_names},
#'   \code{record_ids}, \code{record_names}.
#'
#' @return A tibble of the same shape as \code{partial}, with rows from the
#'   resumed run substituted in for previously incomplete rows.
#'
#' @seealso \code{\link{bulk_upload_files}}, \code{\link{bulk_delete_sessions}},
#'   \code{\link{bulk_delete_folders}}, \code{\link{bulk_delete_files}},
#'   \code{\link{bulk_create_sessions}}, \code{\link{bulk_create_folders}},
#'   \code{\link{bulk_rename_sessions}}, \code{\link{bulk_rename_folders}},
#'   \code{\link{bulk_rename_files}}, \code{\link{bulk_create_records}},
#'   \code{\link{bulk_delete_records}}, \code{\link{bulk_assign_records}},
#'   \code{\link{bulk_unassign_records}}
#'
#' @examples
#' \donttest{
#' \dontrun{
#' result <- tryCatch(
#'   bulk_upload_files(
#'     vol_id = 1, session_id = 42,
#'     file_paths = c("/tmp/a.mp4", "/tmp/b.mp4")
#'   ),
#'   databraryr_bulk_error = function(e) e$partial
#' )
#' # ... fix the failing file, then:
#' result <- resume_bulk(result, bulk_upload_files,
#'                      vol_id = 1, session_id = 42)
#' }
#' }
#' @export
resume_bulk <- function(partial, fn, ..., input_arg = NULL) {
  assertthat::assert_that(
    inherits(partial, "tbl_df"),
    all(c("input", "status") %in% names(partial)),
    msg = "partial must be a tibble produced by a bulk_* function"
  )
  assertthat::assert_that(is.function(fn))

  to_redo <- partial$status %in% c("pending", "failed")
  if (!any(to_redo)) {
    return(partial)
  }

  if (is.null(input_arg)) {
    input_arg <- detect_input_arg(fn)
  }

  remaining <- partial$input[to_redo]
  args <- list(...)
  args[[input_arg]] <- remaining

  new_rows <- do.call(fn, args)

  # Splice new rows back into the original order.
  out <- partial
  redo_idx <- which(to_redo)
  for (k in seq_along(redo_idx)) {
    i <- redo_idx[k]
    out$status[i] <- new_rows$status[k]
    out$result[i] <- new_rows$result[k]
    out$error[i] <- new_rows$error[k]
    out$reason[i] <- new_rows$reason[k]
  }
  out
}

# Internal: infer which formal of a bulk_* function takes the vector of inputs.
#' @noRd
detect_input_arg <- function(fn) {
  candidates <- c(
    "file_paths", "session_ids", "folder_ids", "file_ids",
    "session_names", "folder_names",
    "record_ids", "record_names"
  )
  hit <- intersect(names(formals(fn)), candidates)
  assertthat::assert_that(
    length(hit) == 1,
    msg = paste0(
      "Could not infer input argument of fn; pass `input_arg` explicitly. ",
      "Looked for one of: ", paste(candidates, collapse = ", ")
    )
  )
  hit
}
