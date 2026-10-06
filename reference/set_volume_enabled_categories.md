# Set Enabled Categories for a Volume

Replace the full set of enabled categories for a Databrary volume. This
is a destructive replacement – categories not in the provided list will
be disabled. Pass an empty vector to disable all categories.

## Usage

``` r
set_volume_enabled_categories(
  vol_id = 1,
  category_ids,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- category_ids:

  Integer vector of category IDs to enable.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` on success, `NULL` on failure.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Enable participant (1) and task (6) categories
set_volume_enabled_categories(vol_id = 1, category_ids = c(1, 6))

# Disable all categories
set_volume_enabled_categories(vol_id = 1, category_ids = integer(0))
} # }
# }
```
