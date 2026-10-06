# List affiliates for a user

List affiliates for a user

## Usage

``` r
list_user_affiliates(user_id = 6, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- user_id:

  User identifier. Must be an integer. Default is 6.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

Tibble of affiliates for the user.
