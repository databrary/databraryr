# Assign Record to Session File

Assign a record to a session file, creating a record-file association.
This operation is idempotent - calling it multiple times with the same
parameters will not create duplicate associations.

## Usage

``` r
assign_record_to_file(
  vol_id = 1,
  session_id,
  file_id,
  record_id,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- session_id:

  Numeric session identifier. Must be a positive integer.

- file_id:

  Numeric file identifier. Must be a positive integer.

- record_id:

  Numeric record identifier. Must be a positive integer and must belong
  to the specified volume.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

The response data on success, or `NULL` if the operation fails.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Assign a participant record to a video file
assign_record_to_file(
  vol_id = 1,
  session_id = 10,
  file_id = 20,
  record_id = 123
)
} # }
# }
```
