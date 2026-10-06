# Resume a Bulk Operation After a Failure

When a `bulk_*` run ends with incomplete rows (either because
`on_error = "stop"` threw `databraryr_bulk_error` or because
`on_error = "collect"` left `"failed"` / `"pending"` rows),
`resume_bulk()` re-runs the supplied bulk function on those rows and
merges the result, preserving original input order.

## Usage

``` r
resume_bulk(partial, fn, ..., input_arg = NULL)
```

## Arguments

- partial:

  A tibble produced by a previous `bulk_*` call (typically extracted
  from a `databraryr_bulk_error` condition via `conditionMessage` /
  direct access to `cond$partial`).

- fn:

  The bulk function to re-invoke (e.g. `bulk_upload_files`).

- ...:

  Additional arguments forwarded to `fn` (for example `vol_id`,
  `session_id`, `vb`, `rq`). The remaining inputs are passed as the
  function's input vector argument under the name `input_arg` (default:
  auto-detected from the function's formals).

- input_arg:

  Optional name of the input argument of `fn`. If `NULL` (default), it
  is inferred as the first formal of `fn` matching one of `file_paths`,
  `session_ids`, `folder_ids`, `file_ids`, `session_names`,
  `folder_names`, `record_ids`, `record_names`.

## Value

A tibble of the same shape as `partial`, with rows from the resumed run
substituted in for previously incomplete rows.

## Details

If `fn` itself fast-fails (`on_error = "stop"`), the re-thrown
`databraryr_bulk_error`'s `$partial` is the **full accumulated tibble**
(outcomes from all prior runs merged with the latest attempt), so you
can safely pass it back to `resume_bulk()` in a loop without losing rows
that already succeeded.

## See also

[`bulk_upload_files`](https://databrary.github.io/databraryr/reference/bulk_upload_files.md),
[`bulk_delete_sessions`](https://databrary.github.io/databraryr/reference/bulk_delete_sessions.md),
[`bulk_delete_folders`](https://databrary.github.io/databraryr/reference/bulk_delete_folders.md),
[`bulk_delete_files`](https://databrary.github.io/databraryr/reference/bulk_delete_files.md),
[`bulk_create_sessions`](https://databrary.github.io/databraryr/reference/bulk_create_sessions.md),
[`bulk_create_folders`](https://databrary.github.io/databraryr/reference/bulk_create_folders.md),
[`bulk_rename_sessions`](https://databrary.github.io/databraryr/reference/bulk_rename_sessions.md),
[`bulk_rename_folders`](https://databrary.github.io/databraryr/reference/bulk_rename_folders.md),
[`bulk_rename_files`](https://databrary.github.io/databraryr/reference/bulk_rename_files.md),
[`bulk_create_records`](https://databrary.github.io/databraryr/reference/bulk_create_records.md),
[`bulk_delete_records`](https://databrary.github.io/databraryr/reference/bulk_delete_records.md),
[`bulk_assign_records`](https://databrary.github.io/databraryr/reference/bulk_assign_records.md),
[`bulk_unassign_records`](https://databrary.github.io/databraryr/reference/bulk_unassign_records.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
result <- tryCatch(
  bulk_upload_files(
    vol_id = 1, session_id = 42,
    file_paths = c("/tmp/a.mp4", "/tmp/b.mp4")
  ),
  databraryr_bulk_error = function(e) e$partial
)
# ... fix the failing file, then:
result <- resume_bulk(result, bulk_upload_files,
                     vol_id = 1, session_id = 42)
} # }
# }
```
