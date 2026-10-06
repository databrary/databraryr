# Delete Measure from a Record

Delete a single measure from a record. Note that the backend will reject
attempts to delete measures for required metrics.

## Usage

``` r
delete_record_measure(
  vol_id = 1,
  record_id,
  metric_id,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- record_id:

  Numeric record identifier. Must be a positive integer.

- metric_id:

  Numeric metric identifier. Must be a positive integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` if the measure was successfully deleted, `FALSE` otherwise.
Deletion will fail for required metrics.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Delete a measure
delete_record_measure(
  vol_id = 1,
  record_id = 123,
  metric_id = 5
)

# Delete with verbose output
delete_record_measure(
  vol_id = 1,
  record_id = 123,
  metric_id = 5,
  vb = TRUE
)
} # }
# }
```
