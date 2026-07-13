# Internal helpers shared by bulk_* functions. This file is named
# bulk_apply_internals.R so it loads before other bulk_*.R files (ASCII order).

# Internal: condition constructor for fast-fail with resumable partial state.
#' @noRd
databraryr_bulk_error <- function(message, partial, failed_input) {
  structure(
    class = c("databraryr_bulk_error", "error", "condition"),
    list(
      message = message,
      partial = partial,
      failed_input = failed_input,
      call = sys.call(-1)
    )
  )
}

# Internal: initialise the result tibble with one row per input.
#' @noRd
init_bulk_tibble <- function(inputs) {
  tibble::tibble(
    input = inputs,
    status = rep("pending", length(inputs)),
    result = vector("list", length(inputs)),
    error = rep(NA_character_, length(inputs)),
    reason = rep(NA_character_, length(inputs))
  )
}

# Internal: shared iteration engine. Each pending row runs `fn` with optional
# retries. On failure: `on_error = "stop"` throws `databraryr_bulk_error`;
# `on_error = "collect"` marks the row failed and continues.
#
# `fn` is invoked on each pending input. `is_failure(res)` decides whether the
# returned value (rather than a thrown error) should be treated as a failure.
#' @noRd
bulk_apply <- function(inputs, fn,
                       is_failure = function(res) is.null(res),
                       preflight = NULL,
                       on_error = c("stop", "collect"),
                       max_retries = 0L,
                       retry_delay = 0) {
  on_error <- match.arg(on_error)
  assertthat::assert_that(
    is.numeric(max_retries),
    length(max_retries) == 1L,
    max_retries >= 0,
    max_retries == floor(max_retries),
    msg = "max_retries must be a single non-negative integer"
  )
  assertthat::assert_that(
    is.numeric(retry_delay),
    length(retry_delay) == 1L,
    retry_delay >= 0,
    msg = "retry_delay must be a single non-negative number (seconds)"
  )

  state <- init_bulk_tibble(inputs)

  if (!is.null(preflight)) {
    state <- preflight(state)
  }

  pending_idx <- which(state$status == "pending")
  max_retries <- as.integer(max_retries)

  for (i in pending_idx) {
    attempt <- 0L
    repeat {
      res <- tryCatch(fn(state$input[[i]]), error = function(e) e)
      failed <- inherits(res, "error") || isTRUE(is_failure(res))

      if (!failed) {
        state$status[i] <- "success"
        state$result[[i]] <- res
        break
      }

      msg <- if (inherits(res, "error")) {
        conditionMessage(res)
      } else {
        "operation returned a failure value (NULL or FALSE)"
      }

      if (attempt >= max_retries) {
        state$status[i] <- "failed"
        state$error[i] <- msg
        if (on_error == "stop") {
          stop(databraryr_bulk_error(
            message = sprintf(
              "Bulk operation failed at input '%s': %s",
              format(state$input[[i]]), msg
            ),
            partial = state,
            failed_input = state$input[[i]]
          ))
        }
        break
      }

      attempt <- attempt + 1L
      if (retry_delay > 0) {
        Sys.sleep(as.numeric(retry_delay))
      }
    }
  }

  state
}

# Internal: mark inputs whose basenames already exist in the target session or
# folder as "skipped" with reason "duplicate". `checker` is
# check_duplicate_files_in_session or check_duplicate_files_in_folder; `...` is
# forwarded (vol_id, session_id or folder_id, vb, rq).
#' @noRd
preflight_duplicates <- function(state, checker, ...) {
  filenames <- basename(state$input)
  dupes <- checker(filenames = filenames, ...)
  if (is.null(dupes)) {
    return(state)
  }

  exists_lookup <- stats::setNames(dupes$exists, dupes$filename)
  is_dupe <- !is.na(exists_lookup[filenames]) &
    unname(exists_lookup[filenames])
  is_dupe[is.na(is_dupe)] <- FALSE

  state$status[is_dupe] <- "skipped"
  state$reason[is_dupe] <- "duplicate"
  state
}

# Internal: argument validation -----------------------------------------------

#' @noRd
assert_recyclable <- function(x, n, name = "argument") {
  if (is.null(x)) {
    return(invisible(x))
  }
  assertthat::assert_that(
    length(x) %in% c(1L, n),
    msg = sprintf("%s must be NULL, length 1, or length %d", name, n)
  )
  invisible(x)
}

#' @noRd
recycle_i <- function(x, i) {
  if (is.null(x)) {
    return(NULL)
  }
  if (length(x) == 1L) {
    x[[1L]]
  } else {
    x[[i]]
  }
}

#' @noRd
assert_bulk_names <- function(names_vec, label = "names") {
  assertthat::assert_that(
    is.character(names_vec),
    length(names_vec) >= 1L,
    msg = paste(label, "must be a non-empty character vector")
  )
  assertthat::assert_that(
    !any(is.na(names_vec)),
    msg = paste(label, "must not contain NA")
  )
  trimmed <- trimws(names_vec)
  assertthat::assert_that(
    all(nzchar(trimmed)),
    msg = paste(label, "must not contain empty or whitespace-only names")
  )
  invisible(trimmed)
}

#' @noRd
assert_file_paths <- function(file_paths) {
  assertthat::assert_that(
    is.character(file_paths),
    length(file_paths) >= 1,
    msg = "file_paths must be a non-empty character vector"
  )
  assertthat::assert_that(
    !any(is.na(file_paths)),
    all(nzchar(file_paths)),
    msg = "file_paths must not contain NA or empty strings"
  )
  missing_files <- file_paths[!file.exists(file_paths)]
  assertthat::assert_that(
    length(missing_files) == 0,
    msg = paste0(
      "file_paths point to non-existent files: ",
      paste(missing_files, collapse = ", ")
    )
  )
}

#' @noRd
assert_positive_integer_vec <- function(x, name = deparse(substitute(x))) {
  assertthat::assert_that(
    is.numeric(x),
    length(x) >= 1,
    msg = paste(name, "must be a non-empty numeric vector")
  )
  assertthat::assert_that(
    !any(is.na(x)),
    all(x >= 1),
    all(x == floor(x)),
    msg = paste(name, "must contain only positive integers")
  )
}
