# List Volumes Associated With A User

List Volumes Associated With A User

## Usage

``` r
list_user_volumes(user_id = 6, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- user_id:

  User identifier. Must be a positive integer. Default is 6.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Default is NULL.

## Value

Tibble of volumes the user owns or collaborates on.
