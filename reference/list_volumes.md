# List Volumes Accessible Through The Databrary API.

Returns summary metadata for volumes accessible to the authenticated
user. Results can be filtered by search term or ordering.

## Usage

``` r
list_volumes(
  search = NULL,
  ordering = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- search:

  Optional character string used to filter volumes by title or
  description.

- ordering:

  Optional character string indicating the sort field accepted by the
  API (e.g., `"title"`, `"-title"`).

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble summarizing each accessible volume, or `NULL` when no volumes
match the supplied filters.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volumes(search = "workshop")
} # }
# }
```
