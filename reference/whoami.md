# Retrieve metadata about the authenticated Databrary user.

Calls the Django `/oauth2/test/` endpoint to report the current
authentication method and user profile. Requires a valid OAuth2 access
token acquired via
[`login_db()`](https://databrary.github.io/databraryr/reference/login_db.md).

## Usage

``` r
whoami(refresh = TRUE, vb = options::opt("vb"))
```

## Arguments

- refresh:

  Whether to attempt automatic token refresh when the current access
  token is expired. Defaults to `TRUE`.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

## Value

A list containing `auth_method` and `user` fields (both lists) or `NULL`
if the request fails due to lack of authentication.

## Examples

``` r
if (FALSE) { # \dontrun{
login_db()
whoami()
} # }
```
