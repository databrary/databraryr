# List Assets in a Databrary Session.

\#' @description **\[experimental\]**

## Usage

``` r
list_session_assets(
  session_id = 9807,
  vol_id = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- session_id:

  An integer. A Databrary session number. Default is 9807, the
  "materials" folder from Databrary volume 1.

- vol_id:

  Optional integer. The volume containing the session. Recent versions
  of the Databrary API require this value to be supplied because session
  identifiers are scoped to volumes.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. If NULL, a default request is generated
  from databraryr::make_default_request().

## Value

A data frame with information about all assets in a volume.

## Details

As of v0.6.3 `list_session_assets()` replaces an older function that is
now named
[`list_volume_session_assets()`](https://databrary.github.io/databraryr/reference/list_volume_session_assets.md).
The older function requires both a volume ID and a session ID. The new
function requires only a session ID.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
list_session_assets() # Session 9807 in volume 1
} # }
# }
```
