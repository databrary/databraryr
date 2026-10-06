# List authorized investigators for an institution

Lists the authorized investigators at an institution.

## Usage

``` r
list_authorized_investigators(
  institution_id = 12,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- institution_id:

  Institution identifier. Must be a positive integer. Default is 12.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

Tibble of investigators; NULL if none.
