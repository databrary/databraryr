# Get Tag Information By ID

Retrieve detailed information about a specific tag from Databrary using
its unique identifier.

## Usage

``` r
get_tag_by_id(tag_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- tag_id:

  Numeric tag identifier. Must be a positive integer.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the tag's metadata including id and name, or `NULL` if the
tag is not found or inaccessible.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Get details for a specific tag
get_tag_by_id(tag_id = 1)

# Get tag information with verbose output
get_tag_by_id(tag_id = 1, vb = TRUE)
} # }
# }
```
