# Get public profile information for a Databrary user

Get public profile information for a Databrary user

## Usage

``` r
get_user_by_id(user_id = 6, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- user_id:

  User identifier. Must be a positive integer. Default is 6.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the user's public metadata.
