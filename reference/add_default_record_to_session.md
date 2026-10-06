# Add a Default Record to a Session

Attach a record to a session as a default record. Default records apply
to all files in the session unless overridden. The record must either
belong to the destination volume or be accessible to it via a linked
volume; the server enforces this and returns `403` otherwise.

## Usage

``` r
add_default_record_to_session(
  vol_id = 1,
  session_id,
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

- record_id:

  Numeric record identifier. Must be a positive integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` if the record was successfully added, `FALSE` otherwise.

## See also

[`remove_default_record_from_session`](https://databrary.github.io/databraryr/reference/remove_default_record_from_session.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
add_default_record_to_session(vol_id = 1, session_id = 42, record_id = 101)
} # }
# }
```
