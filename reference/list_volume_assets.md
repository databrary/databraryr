# List Assets in Databrary Volume.

List Assets in Databrary Volume.

## Usage

``` r
list_volume_assets(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is NULL.

## Value

A tibble with one row per asset. Columns `asset_duration` and
`asset_thumbnail_url` are always present (as `NA` when the API omits
them). Other fields come from the volume sessions/files payload.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_assets() # Assets in volume 1
} # }
# }
```
