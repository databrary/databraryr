# List Account Activity For A Databrary User.

Retrieve the OAuth and login activity history for a specific user.
Access is restricted to administrators and authorized investigators with
sufficient privileges.

## Usage

``` r
list_user_history(user_id = 22582, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- user_id:

  Target user identifier. Must be a positive integer. Default is 22582.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing authentication and activity events for the selected
user, or `NULL` when no entries are available.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_user_history(user_id = 22582)
} # }
# }
```
