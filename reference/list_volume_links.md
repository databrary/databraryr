# Retrieves URL Links From A Databrary Volume.

Retrieves URL Links From A Databrary Volume.

## Usage

``` r
list_volume_links(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object.

## Value

A data frame with the requested data.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_links() # Links from volume 1
} # }
# }
```
