# Delete Folder from Databrary Volume

Delete (soft-delete) a folder from a Databrary volume. The folder and
its associated metadata are marked as deleted but not permanently
removed from the database.

## Usage

``` r
delete_folder(vol_id = 1, folder_id, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- folder_id:

  Numeric folder identifier. Must be a positive integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` if the folder was successfully deleted, `FALSE` otherwise.

## See also

[`create_folder`](https://databrary.github.io/databraryr/reference/create_folder.md),
[`update_folder`](https://databrary.github.io/databraryr/reference/update_folder.md),
[`patch_folder`](https://databrary.github.io/databraryr/reference/patch_folder.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
delete_folder(vol_id = 1, folder_id = 42)
} # }
# }
```
