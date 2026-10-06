# Get Enabled Categories for a Volume

Retrieve the list of categories currently enabled for a Databrary
volume. Returns the `enabled_categories` field from the volume detail
endpoint.

## Usage

``` r
get_volume_enabled_categories(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list of category objects (each with `id`, `name`, `metrics`, etc.), or
`NULL` if the volume is not found.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
get_volume_enabled_categories(vol_id = 1)
} # }
# }
```
