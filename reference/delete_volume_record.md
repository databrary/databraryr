# Delete Record from Databrary Volume

Delete (soft-delete) a record from a Databrary volume. The record and
its measures are marked as deleted but not permanently removed from the
database.

## Usage

``` r
delete_volume_record(vol_id = 1, record_id, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- record_id:

  Numeric record identifier. Must be a positive integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` if the record was successfully deleted, `FALSE` otherwise.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Delete a record
delete_volume_record(vol_id = 1, record_id = 123)

# Delete with verbose output
delete_volume_record(vol_id = 1, record_id = 123, vb = TRUE)
} # }
# }
```
