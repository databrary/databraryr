# List Databrary Users.

Retrieve directory metadata for Databrary users. Results can be filtered
by name or restricted to specific account types using optional
parameters.

## Usage

``` r
list_users(
  search = NULL,
  include_suspended = NULL,
  exclude_self = NULL,
  is_authorized_investigator = NULL,
  has_api_access = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- search:

  Optional character string used to filter results by name or email
  address.

- include_suspended:

  Optional logical value. When `TRUE`, suspended accounts are included
  in the response.

- exclude_self:

  Optional logical value. When `TRUE`, the authenticated user is omitted
  from the results.

- is_authorized_investigator:

  Optional logical value restricting the response to authorized
  investigators.

- has_api_access:

  Optional logical value restricting the response to accounts with API
  access enabled.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing directory metadata for each user, or `NULL` when no
results are available for the supplied filters.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_users(search = "gilmore")
} # }
# }
```
