# Show Databrary Release Levels

Show Databrary Release Levels

## Usage

``` r
get_release_levels(vb = options::opt("vb"))
```

## Arguments

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

## Value

A data frame with Databrary's release levels.

## Examples

``` r
# \donttest{
get_release_levels()
#> [1] "private"            "authorized_users"   "learning_audiences"
#> [4] "public"            
# }
```
