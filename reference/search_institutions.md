# Search For Institutions In Databrary.

Perform a search across institutions registered with Databrary.

## Usage

``` r
search_institutions(search_string, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- search_string:

  Character string describing the institution search query.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing matching institutions ordered by relevance, or
`NULL` when no matches exist for the query.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
search_institutions("state")
} # }
# }
```
