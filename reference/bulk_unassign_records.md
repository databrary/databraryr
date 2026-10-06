# Bulk Unassign Default Records from a Session

For each record id, call
[`remove_default_record_from_session`](https://databrary.github.io/databraryr/reference/remove_default_record_from_session.md).

## Usage

``` r
bulk_unassign_records(
  vol_id = 1,
  session_id,
  record_ids,
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

  Numeric session identifier.

- record_ids:

  Numeric vector of record identifiers.

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
`input` is each record id.

## See also

[`remove_default_record_from_session`](https://databrary.github.io/databraryr/reference/remove_default_record_from_session.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
bulk_unassign_records(vol_id = 1, session_id = 42, record_ids = c(101, 102))
} # }
# }
```
