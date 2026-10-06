# Delete Session from Databrary Volume

Delete (soft-delete) a session from a Databrary volume. The session and
its associated metadata are marked as deleted but not permanently
removed from the database.

## Usage

``` r
delete_session(vol_id = 1, session_id, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- session_id:

  Numeric session identifier. Must be a positive integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` if the session was successfully deleted, `FALSE` otherwise.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Delete a session
delete_session(vol_id = 1, session_id = 42)

# Delete with verbose output
delete_session(vol_id = 1, session_id = 42, vb = TRUE)
} # }
# }
```
