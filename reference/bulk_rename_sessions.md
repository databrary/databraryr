# Bulk Rename Sessions in a Databrary Volume

Rename many sessions sequentially via
[`patch_session`](https://databrary.github.io/databraryr/reference/patch_session.md)
(`name` only). `session_ids` must be unique so each row maps to one
name. With
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md),
pass `new_names` aligned to the subset of ids being retried (same order
as incomplete rows).

## Usage

``` r
bulk_rename_sessions(
  vol_id = 1,
  session_ids,
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

- session_ids:

  Numeric vector of session identifiers (unique positive integers).

- new_names:

  Character vector of new names, same length as `session_ids`.

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
`input` is each session id.

## See also

[`patch_session`](https://databrary.github.io/databraryr/reference/patch_session.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
bulk_rename_sessions(
  vol_id = 1,
  session_ids = c(101, 102),
  new_names = c("Lab visit A", "Lab visit B")
)
} # }
# }
```
