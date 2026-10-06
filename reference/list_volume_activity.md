# List Activity In A Databrary Volume

If a user has access to a volume, this command lists the modification
history of the volume.

## Usage

``` r
list_volume_activity(vol_id = NULL, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- vol_id:

  Selected volume number. Must be a positive integer. Default is NULL.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to NULL.

## Value

A list with the activity history on a volume.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# The following will only return output if the user has *write* privileges
# on the volume.

list_volume_activity(vol_id)
} # }
# }
```
