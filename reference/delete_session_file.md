# Delete a Session File from a Databrary Volume

Delete (soft-delete) a file from a Databrary session. The file is marked
as deleted on the server but not permanently removed. A second delete on
the same file returns `FALSE`, not an error.

## Usage

``` r
delete_session_file(
  vol_id = 1,
  session_id,
  file_id,
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

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` if the file was successfully deleted, `FALSE` otherwise.

## See also

[`update_session_file`](https://databrary.github.io/databraryr/reference/update_session_file.md),
[`patch_session_file`](https://databrary.github.io/databraryr/reference/patch_session_file.md),
[`get_session_file`](https://databrary.github.io/databraryr/reference/get_session_file.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
delete_session_file(vol_id = 1, session_id = 42, file_id = 99)
} # }
# }
```
