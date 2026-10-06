# List Assets in a Session from a Databrary volume.

\#' @description **\[experimental\]**

## Usage

``` r
list_volume_session_assets(
  vol_id = 2,
  session_id = 11,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- session_id:

  The session number in the selected volume.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object.

## Value

A data frame with information about all assets in a volume.

## Details

`list_volume_session_assets()` is a new name for the \<v0.6.2
[`list_session_assets()`](https://databrary.github.io/databraryr/reference/list_session_assets.md)
as function. There is a new
[`list_session_assets()`](https://databrary.github.io/databraryr/reference/list_session_assets.md)
function that does not requre the volume ID. The
`list_volume_session_assets()` *requires* a volume ID.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_volume_session_assets() # Defaults to session 11 in volume 2
} # }
# }
```
