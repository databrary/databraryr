# List Records in Databrary Volume

Retrieve all records (participant data with measures) from a specific
Databrary volume. Records contain participant information including age,
birthday, category, and associated measures collected during sessions.

## Usage

``` r
list_volume_records(
  vol_id = 1,
  category_id = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- category_id:

  Optional numeric category identifier to filter records by category
  type.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing metadata for each record (aligned with core
`RecordSerializer`): ids, owning volume (`record_volume`), owning volume
name (`record_volume_name`), category, measures, birthday, age columns,
default sessions (`record_default_sessions`, list column of
`id`/`name`), and linked-content provenance (`record_source_kind`). The
`record_volume` column may differ from `vol_id` when the volume lists
linked records from other volumes. Returns an empty tibble when the
volume has no records, or `NULL` when the API call fails (e.g.
non-existent volume).

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# List all records in volume 1
list_volume_records(vol_id = 1)

# Filter records by category
list_volume_records(vol_id = 1, category_id = 2)

# With verbose output
list_volume_records(vol_id = 1, vb = TRUE)
} # }
# }
```
