# Get Volume Collaborator By ID

Retrieve detailed information about a specific collaborator on a
Databrary volume using their unique collaborator identifier. Returns
collaborator details including user information, sponsor details, access
level, and visibility settings.

## Usage

``` r
get_volume_collaborator_by_id(
  vol_id = 1,
  collaborator_id = 1,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer. Default is 1.

- collaborator_id:

  Numeric collaborator identifier. Must be a positive integer. Default
  is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the collaborator's metadata including id, volume, user
details, sponsor information (if applicable), access level, visibility
settings, and expiration date, or `NULL` if the collaborator is not
found or inaccessible.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Get details for a specific collaborator
get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 5)

# Get collaborator information with verbose output
get_volume_collaborator_by_id(vol_id = 1, collaborator_id = 5, vb = TRUE)
} # }
# }
```
