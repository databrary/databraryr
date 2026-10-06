# Bulk Create Records in a Databrary Volume

Create many records sequentially via
[`create_volume_record`](https://databrary.github.io/databraryr/reference/create_volume_record.md).
The resolved name metric is set from each `record_names` entry
automatically. `record_names` must be unique (for
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)).
`category_id` and `measures` may each be length 1 (recycled to every
row) or match `length(record_names)`.

## Usage

``` r
bulk_create_records(
  vol_id = 1,
  record_names,
  category_id,
  measures = NULL,
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

- record_names:

  Non-empty character vector of record display names (trimmed).

- category_id:

  Numeric category id(s); length 1 or `length(record_names)`.

- measures:

  Optional named list of additional metric values (recycled when length
  1), or a list-of-lists with one element per `record_names` entry. See
  [`create_volume_record`](https://databrary.github.io/databraryr/reference/create_volume_record.md)
  for value types.

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
`input` is each trimmed record name.

## Details

With
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md),
pass `measures` aligned to the subset of `record_names` being retried
(same order as incomplete rows), analogous to `new_names` in
[`bulk_rename_sessions`](https://databrary.github.io/databraryr/reference/bulk_rename_sessions.md).

## See also

[`create_volume_record`](https://databrary.github.io/databraryr/reference/create_volume_record.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
bulk_create_records(
  vol_id = 1,
  record_names = c("P101", "P102"),
  category_id = 6
)

bulk_create_records(
  vol_id = 1,
  record_names = c("P101", "P102"),
  category_id = 6,
  measures = list("30" = "Control")
)
} # }
# }
```
