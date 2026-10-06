# List Sessions in Databrary Volume.

List Sessions in Databrary Volume.

## Usage

``` r
list_volume_sessions(
  vol_id = 1,
  include_vol_data = FALSE,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- include_vol_data:

  A Boolean value. Include volume-level metadata or not. Default is
  FALSE.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. If NULL (the default) a request will be
  generated, but this will only permit public information to be
  returned.

## Value

A data frame with information about all assets in a volume.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_sessions() # Sessions in Volume 1
} # }
# }
```
