# Extract Databrary Permission Levels.

Extract Databrary Permission Levels.

## Usage

``` r
get_permission_levels(vb = options::opt("vb"))
```

## Arguments

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

## Value

An array with the permission levels that can be assigned to data.

## Examples

``` r
# \donttest{
get_permission_levels()
#> [1] "superuser"          "owner"              "investigator"      
#> [4] "read write"         "read only"          "read only shared"  
#> [7] "read only public"   "read only overview" "none"              
# }
```
