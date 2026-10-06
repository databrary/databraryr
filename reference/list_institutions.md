# List Institutions

Retrieve a list of all institutions registered with Databrary.
Optionally filter by search string.

## Usage

``` r
list_institutions(search_string = NULL, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- search_string:

  Optional character string to filter institutions. If `NULL` (the
  default), returns all institutions.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble containing institutions with their metadata including id, name,
url, date_signed, source, created_at, updated_at, has_avatar,
has_administrators, latitude, longitude, and manual_coordinates, or
`NULL` if no institutions are found.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# List all institutions
list_institutions()

# List institutions filtered by search string
list_institutions(search_string = "university")

# With verbose output
list_institutions(vb = TRUE)
} # }
# }
```
