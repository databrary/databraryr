# Get Summary Data About A Databrary Volume

Get Summary Data About A Databrary Volume

## Usage

``` r
get_volume_by_id(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Volume ID. Must be a positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. If NULL (the default), a new request is
  generated using
  [`make_default_request()`](https://databrary.github.io/databraryr/reference/make_default_request.md).
  To access restricted data, the user must login with a specific request
  object using
  [`login_db()`](https://databrary.github.io/databraryr/reference/login_db.md).

## Value

A tibble with summary information about a volume.

## Examples

``` r
if (FALSE) { # \dontrun{
get_volume_by_id() # Default is Volume 1
} # }
```
