# List sponsorships for a user

List sponsorships for a user

## Usage

``` r
list_user_sponsors(user_id = 6, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- user_id:

  User identifier.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

Tibble of sponsors for the user.
