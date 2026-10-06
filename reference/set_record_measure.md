# Set Measure for a Record

Create or update a single measure for a record. This performs an upsert
operation - if the measure exists for this metric, it is updated;
otherwise, a new measure is created.

## Usage

``` r
set_record_measure(
  vol_id = 1,
  record_id,
  metric_id,
  value,
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

- value:

  The measure value. Can be a string (for text metrics), a number (for
  numeric metrics), or a list with `year`, `month`, `day`, optional
  `month` and `day` fields (for date metrics).

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

The measure data on success, or `NULL` if the operation fails.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Set a text measure
set_record_measure(
  vol_id = 1,
  record_id = 123,
  metric_id = 2,
  value = "Female"
)

# Set a numeric measure
set_record_measure(
  vol_id = 1,
  record_id = 123,
  metric_id = 5,
  value = 24.5
)

# Set a date measure
set_record_measure(
  vol_id = 1,
  record_id = 123,
  metric_id = 4,
  value = list(year = 2020, month = 3, day = 15)
)
} # }
# }
```
