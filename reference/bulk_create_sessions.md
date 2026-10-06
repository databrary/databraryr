# Bulk Create Sessions in a Databrary Volume

Create many sessions sequentially. `input` in the result tibble is the
session name for that row (after trimming). `session_names` must be
unique. Optional fields are recycled. Structured `date`,
`date_precision`, and `default_records` are not supported here; use
[`create_session`](https://databrary.github.io/databraryr/reference/create_session.md)
per row if you need them.

## Usage

``` r
bulk_create_sessions(
  vol_id = 1,
  session_names,
  release_level = NULL,
  source_date = NULL,
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

- session_names:

  Non-empty character vector of session names.

- release_level:

  Optional character vector (length 1 or `length(session_names)`).

- source_date:

  Optional `Date`, ISO string, or vector thereof (length 1 or
  `length(session_names)`).

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
[`bulk_upload_files`](https://databrary.github.io/databraryr/reference/bulk_upload_files.md),
with `input` equal to the trimmed session name for each row.

## See also

[`create_session`](https://databrary.github.io/databraryr/reference/create_session.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
bulk_create_sessions(vol_id = 1, session_names = c("S1", "S2"))
} # }
# }
```
