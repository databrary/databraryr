# Get Funder Information By ID

Retrieve detailed information about a specific funder from Databrary
using its unique identifier.

## Usage

``` r
get_funder_by_id(funder_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- funder_id:

  Numeric funder identifier. Must be a positive integer.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the funder's metadata including id, name, and approval
status, or `NULL` if the funder is not found or inaccessible.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Get details for a specific funder
get_funder_by_id(funder_id = 1)

# Get funder information with verbose output
get_funder_by_id(funder_id = 1, vb = TRUE)
} # }
# }
```
