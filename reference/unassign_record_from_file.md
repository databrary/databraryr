# Unassign Record from Session File

Remove the association between a record and a session file. If the
record is not assigned to the file, this operation will fail.

## Usage

``` r
unassign_record_from_file(
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

  Numeric record identifier. Must be a positive integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` if the record was successfully unassigned, `FALSE` otherwise.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Unassign a record from a file
unassign_record_from_file(
  vol_id = 1,
  session_id = 10,
  file_id = 20,
  record_id = 123
)
} # }
# }
```
