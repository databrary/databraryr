# List Collaborators On A Databrary Volume.

Retrieve collaboration metadata for a specified volume, including
sponsor details and access levels.

## Usage

``` r
list_volume_collaborators(vol_id = 1, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A tibble summarizing collaborator relationships on the volume, or `NULL`
when no collaborators are associated with the volume.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_collaborators(vol_id = 1)
} # }
# }
```
