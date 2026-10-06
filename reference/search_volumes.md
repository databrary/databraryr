# Search For Volumes In Databrary.

Search across Databrary volumes using the Django search endpoint.

## Usage

``` r
search_volumes(search_string, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- search_string:

  Character string describing the volume search query.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing matching volumes ordered by relevance, or `NULL`
when no matches exist for the query.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
search_volumes("workshop")
} # }
# }
```
