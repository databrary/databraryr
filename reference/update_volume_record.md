# Update Record in Databrary Volume

Sends a PATCH request to update a record. The HTTP method is partial in
principle, but the API validates `measures` as a complete snapshot of
**required** metrics for the record's category: every required metric
must appear in `measures`, or the server returns `400`
(`Missing required measures...`). The server then replaces stored values
for each metric key you send (it does not merge your list with existing
measures before that check). To change a single metric without
assembling the full required set, use
[`set_record_measure`](https://databrary.github.io/databraryr/reference/set_record_measure.md).

## Usage

``` r
update_volume_record(
  vol_id = 1,
  record_id,
  measures = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- record_id:

  Numeric record identifier. Must be a positive integer.

- measures:

  Optional named list mapping metric IDs (as strings) to values. Values
  can be strings (for text metrics), numbers (for numeric metrics), or
  lists with `year` and optional `month`, `day` fields (for date
  metrics). When supplied, must include all required metrics for the
  record's category (use
  [`get_volume_enabled_categories`](https://databrary.github.io/databraryr/reference/get_volume_enabled_categories.md)
  or
  [`get_volume_record_by_id`](https://databrary.github.io/databraryr/reference/get_volume_record_by_id.md)
  to discover ids and current values).

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the updated record's metadata (same shape as
[`get_volume_record_by_id`](https://databrary.github.io/databraryr/reference/get_volume_record_by_id.md)),
or `NULL` if update fails.

## See also

[`set_record_measure`](https://databrary.github.io/databraryr/reference/set_record_measure.md),
[`get_volume_record_by_id`](https://databrary.github.io/databraryr/reference/get_volume_record_by_id.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Prefer for a single metric (no need to list all required measures)
set_record_measure(vol_id = 1, record_id = 123, metric_id = 2, value = "Male")

# PATCH with measures: merge current values with changes so required metrics stay present
rec <- get_volume_record_by_id(vol_id = 1, record_id = 123)
new_measures <- utils::modifyList(rec$measures, list("2" = "Updated label"))
update_volume_record(vol_id = 1, record_id = 123, measures = new_measures)
} # }
# }
```
