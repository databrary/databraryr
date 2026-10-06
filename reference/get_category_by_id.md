# Get Category Information By ID

Retrieve detailed information about a specific category from Databrary
using its unique identifier. Categories include nested metrics that
define data collection fields.

## Usage

``` r
get_category_by_id(category_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- category_id:

  Numeric category identifier. Must be a positive integer.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the category's metadata including id, name, description, and
nested metrics, or `NULL` if the category is not found or inaccessible.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Get details for a specific category
get_category_by_id(category_id = 1)

# Get category information with verbose output
get_category_by_id(category_id = 1, vb = TRUE)
} # }
# }
```
