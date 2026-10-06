# Search For Users In Databrary.

Perform a directory search across Databrary users by name or email
address.

## Usage

``` r
search_users(search_string, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- search_string:

  Character string describing the search query.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing user matches ordered by relevance, or `NULL` when no
matches exist for the query.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
search_users("gilmore")
} # }
# }
```
