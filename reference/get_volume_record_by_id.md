# Get Volume Record By ID

Retrieve detailed information about a specific record (participant data)
from a Databrary volume using its unique identifier. Records contain
participant information including age, birthday, category, and
associated measures collected during sessions.

## Usage

``` r
get_volume_record_by_id(
  vol_id = 1,
  record_id = 1,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- record_id:

  Numeric record identifier. Must be a positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list aligned with the API record payload: `record_id`,
`record_volume`, `record_volume_name`, `record_category_id`, `measures`,
`birthday`, `age`, `default_sessions` (session ids/names where this
record is a default), `record_source_kind` (linked-content provenance,
e.g. `native`, `source_linked_file`). `record_volume` may differ from
`vol_id` for linked records. Returns `NULL` if the record is not found
or inaccessible.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Get details for a specific record
get_volume_record_by_id(vol_id = 1, record_id = 123)

# Get record information with verbose output
get_volume_record_by_id(vol_id = 1, record_id = 123, vb = TRUE)
} # }
# }
```
