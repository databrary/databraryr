# Lists Keywords And Tags For A Volume.

Lists Keywords And Tags For A Volume.

## Usage

``` r
list_volume_tags(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is NULL.

## Value

A data frame with the requested data.

## Examples

``` r
if (FALSE) { # \dontrun{
list_volume_tags()
} # }
```
