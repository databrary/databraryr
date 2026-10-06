# List Databrary Categories

Retrieve all available categories from Databrary. Categories define
different types of data collection sessions and include nested metrics
that specify the data fields collected for each category.

## Usage

``` r
list_categories(vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing metadata for each category including id, name,
description, and nested metrics, or `NULL` when no results are
available.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# List all categories
list_categories()

# List with verbose output
list_categories(vb = TRUE)
} # }
# }
```
