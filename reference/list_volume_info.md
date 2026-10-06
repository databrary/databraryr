# List Basic Volume Info.

List Basic Volume Info.

## Usage

``` r
list_volume_info(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Defaults to 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. If NULL (the default). a request will be
  generated, but this will only permit public information to be
  returned.

## Value

A data frame with basic information about a volume.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_info() # Sessions in Volume 1
} # }
# }
```
