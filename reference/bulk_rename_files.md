# Bulk Rename Files in a Databrary Session

Rename many session files sequentially via
[`patch_session_file`](https://databrary.github.io/databraryr/reference/patch_session_file.md)
(`name` only). `file_ids` must be unique. Folder assets are not
supported here (no folder file PATCH helper). With
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md),
pass `new_names` aligned to incomplete rows.

## Usage

``` r
bulk_rename_files(
  vol_id = 1,
  session_id,
  file_ids,
  new_names,
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

  Numeric session identifier containing the files.

- file_ids:

  Numeric vector of file identifiers (unique positive integers).

- new_names:

  Character vector of new names, same length as `file_ids`.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

- on_error:

  `"stop"` or `"collect"`; see
  [`bulk_upload_files`](https://databrary.github.io/databraryr/reference/bulk_upload_files.md).

- max_retries:

  Non-negative integer; extra attempts per input after the first
  failure.

- retry_delay:

  Seconds between retries.

## Value

A `tibble` as documented in
[`bulk_upload_files`](https://databrary.github.io/databraryr/reference/bulk_upload_files.md);
`input` is each file id.

## See also

[`patch_session_file`](https://databrary.github.io/databraryr/reference/patch_session_file.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
bulk_rename_files(
  vol_id = 1,
  session_id = 42,
  file_ids = c(1001, 1002),
  new_names = c("clip_a.mp4", "clip_b.mp4")
)
} # }
# }
```
