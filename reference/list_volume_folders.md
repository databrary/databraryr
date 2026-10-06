# List Folders in a Databrary Volume.

List Folders in a Databrary Volume.

## Usage

``` r
list_volume_folders(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble with metadata about folders in the selected volume, or `NULL`
when no folders are available.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_folders() # Folders in volume 1
} # }
# }
```
