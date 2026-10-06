# Download Multiple Assets From a Session Data Frame.

Iterates over a data frame of session assets, requesting signed download
links for each asset and saving them to disk. Designed to work with
[`list_session_assets()`](https://databrary.github.io/databraryr/reference/list_session_assets.md)
or
[`list_volume_session_assets()`](https://databrary.github.io/databraryr/reference/list_volume_session_assets.md)
output.

## Usage

``` r
download_session_assets_fr_df(
  session_df = list_session_assets(session_id = 9224, vol_id = 1),
  target_dir = tempdir(),
  add_session_subdir = TRUE,
  overwrite = TRUE,
  make_portable_fn = FALSE,
  timeout_secs = REQUEST_TIMEOUT_VERY_LONG,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- session_df:

  Data frame describing assets. Must include `vol_id`, `session_id`,
  `asset_id`, and `asset_name` columns. Defaults to the result of
  `list_session_assets(session_id = 9224, vol_id = 1)`. Explicit `NULL`
  triggers the same call using the current `vb` and `rq`.

- target_dir:

  Character string. Base directory for downloads. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- add_session_subdir:

  Logical. When `TRUE`, creates a subdirectory per session inside
  `target_dir`.

- overwrite:

  Logical. When `FALSE`, the function aborts if the target directory
  already exists.

- make_portable_fn:

  Logical. When `TRUE`, filenames are sanitized via
  [`make_fn_portable()`](https://databrary.github.io/databraryr/reference/make_fn_portable.md).

- timeout_secs:

  Numeric. Timeout applied to each download request.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An optional `httr2` request object reused when requesting signed
  links.

## Value

Character vector of downloaded file paths or `NULL` if the request fails
before any downloads start.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
assets <- list_session_assets(vol_id = 1, session_id = 9224)
download_session_assets_fr_df(assets, vb = TRUE)
} # }
# }
```
