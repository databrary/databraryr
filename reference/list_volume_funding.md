# Lists Funders Associated With a Databrary Volume.

Lists Funders Associated With a Databrary Volume.

## Usage

``` r
list_volume_funding(
  vol_id = 1,
  add_id = TRUE,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number.

- add_id:

  A logical value. Include the volume ID in the output. Default is TRUE.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object.

## Value

A data frame with funder information for the volume.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_funding() # Funding for volume 1

list_volume_funding(vol_id = c(1:10))
} # }
# }
```
