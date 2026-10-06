# List Assets Within a Databrary Folder.

List Assets Within a Databrary Folder.

## Usage

``` r
list_folder_assets(
  folder_id = 9807,
  vol_id = 1,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- folder_id:

  Folder identifier scoped to the given volume. Must be a positive
  integer. Default is 9807.

- vol_id:

  Volume containing the folder. Required for Django API calls. Must be a
  positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble with metadata for files contained in the folder, or `NULL` when
the folder has no accessible assets.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_folder_assets(folder_id = 1, vol_id = 1)
} # }
# }
```
