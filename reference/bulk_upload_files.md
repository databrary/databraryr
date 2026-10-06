# Bulk Upload Files to a Databrary Session or Folder

Upload many files sequentially to a single session or a single folder.
Optionally runs a preflight check for duplicate basenames and skips
those uploads. With `on_error = "stop"` (default), fails fast on the
first error and throws `databraryr_bulk_error` with a partial tibble for
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md).

## Usage

``` r
bulk_upload_files(
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
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- session_id:

  Session id when uploading to a session. Exactly one of `session_id`
  and `folder_id` must be non-`NULL`.

- file_paths:

  Character vector of local file paths to upload. Each must point to an
  existing file.

- folder_id:

  Folder id when uploading to a folder. Exactly one of `session_id` and
  `folder_id` must be non-`NULL`.

- preflight:

  Logical; if `TRUE` (default), skip files whose basename already exists
  in the target session or folder (via
  [`check_duplicate_files_in_session`](https://databrary.github.io/databraryr/reference/check_duplicate_files_in_session.md)
  or
  [`check_duplicate_files_in_folder`](https://databrary.github.io/databraryr/reference/check_duplicate_files_in_folder.md)).

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

- on_error:

  `"stop"` (default) throws `databraryr_bulk_error` on first failure
  after retries; `"collect"` marks failed rows and continues.

- max_retries:

  Non-negative integer: extra attempts per input after the first failure
  (default `0`).

- retry_delay:

  Seconds to sleep between retries (default `0`).

## Value

A `tibble` with one row per input file and columns: `input` (path),
`status` (`"success"`, `"failed"`, `"skipped"`, or `"pending"`),
`result` (list-column with the per-file API response or `NULL`), `error`
(character message on failure), and `reason` (e.g. `"duplicate"`).

## See also

[`upload_file`](https://databrary.github.io/databraryr/reference/upload_file.md),
[`check_duplicate_files_in_session`](https://databrary.github.io/databraryr/reference/check_duplicate_files_in_session.md),
[`check_duplicate_files_in_folder`](https://databrary.github.io/databraryr/reference/check_duplicate_files_in_folder.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
result <- bulk_upload_files(
  vol_id = 1,
  session_id = 42,
  file_paths = c("/tmp/a.mp4", "/tmp/b.mp4")
)
result <- bulk_upload_files(
  vol_id = 1,
  file_paths = c("/tmp/a.mp4", "/tmp/b.mp4"),
  folder_id = 7
)
} # }
# }
```
