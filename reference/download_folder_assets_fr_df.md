# Download Multiple Assets From a Folder Data Frame.

Iterates over a data frame of folder assets, requesting signed download
links for each asset and saving them to disk. Designed to work with
[`list_folder_assets()`](https://databrary.github.io/databraryr/reference/list_folder_assets.md)
output.

## Usage

``` r
download_folder_assets_fr_df(
  folder_df = list_folder_assets(vol_id = 1),
  target_dir = tempdir(),
  add_folder_subdir = TRUE,
  overwrite = TRUE,
  make_portable_fn = FALSE,
  timeout_secs = REQUEST_TIMEOUT_VERY_LONG,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- folder_df:

  Data frame describing assets. Must include `vol_id`, `folder_id`,
  `asset_id`, and `asset_name` columns. Defaults to the result of
  `list_folder_assets(vol_id = 1)`. Explicit `NULL` triggers the same
  call using the current `vb` and `rq`.

- target_dir:

  Character string. Base directory for downloads. Defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- add_folder_subdir:

  Logical. When `TRUE`, creates a subdirectory per folder inside
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
assets <- list_folder_assets(folder_id = 1, vol_id = 1)
download_folder_assets_fr_df(assets, vb = TRUE)
} # }
# }
```
