# Get statistics for an institution

Retrieve aggregated statistics for an institution including volume
count, file count, and data footprints. Returns NULL (204 No Content)
when statistics have not been computed yet.

## Usage

``` r
get_institution_statistics(
  institution_id = 1,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- institution_id:

  Institution identifier. Must be a positive integer.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with institution statistics including:

- `institution_id`: The institution identifier

- `volumes_number`: Number of volumes

- `files_number`: Number of files

- `uploaded_data_footprint`: Size of uploaded data in bytes

- `transcoded_data_footprint`: Size of transcoded data in bytes

- `soft_deleted_uploaded_data_footprint`: Size of soft-deleted uploaded
  data

- `soft_deleted_transcoded_data_footprint`: Size of soft-deleted
  transcoded data

- `created_at`: Timestamp when statistics were created

- `updated_at`: Timestamp when statistics were last updated Returns NULL
  when statistics have not been computed or institution is not found.
