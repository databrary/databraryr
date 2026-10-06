# Enable a Category for a Volume

Add a single category to a volume's enabled set. This is additive –
existing enabled categories are preserved. No-op if the category is
already enabled.

## Usage

``` r
enable_volume_category(
  vol_id = 1,
  category_id,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- category_id:

  Numeric category identifier to enable.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

`TRUE` on success (or if already enabled), `NULL` on failure.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
enable_volume_category(vol_id = 1, category_id = 1)
} # }
# }
```
