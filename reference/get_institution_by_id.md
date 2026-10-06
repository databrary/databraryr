# Get institution metadata

Get institution metadata

## Usage

``` r
get_institution_by_id(institution_id = 12, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- institution_id:

  Institution identifier. Must be a positive integer.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

List of institution metadata or NULL when inaccessible.
