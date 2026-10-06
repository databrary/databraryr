# Download a Single Folder Asset From a Data Frame Row.

Helper used by
[`download_folder_assets_fr_df()`](https://databrary.github.io/databraryr/reference/download_folder_assets_fr_df.md)
to fetch a single asset via the signed-download workflow.

## Usage

``` r
download_folder_asset_from_df(
  i = NULL,
  folder_df = NULL,
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

- i:

  Integer. Index of the asset within `folder_df`.

- folder_df:

  Data frame containing folder asset metadata.

- target_dir:

  Base directory for downloads.

- add_folder_subdir:

  Logical. When `TRUE`, creates a subdirectory per folder inside
  `target_dir`.

- overwrite:

  Logical. When `FALSE`, existing files are saved with a timestamped
  suffix.

- make_portable_fn:

  Logical. When `TRUE`, filenames are sanitized via
  [`make_fn_portable()`](https://databrary.github.io/databraryr/reference/make_fn_portable.md).

- timeout_secs:

  Numeric. Timeout applied to the signed download request.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  Optional `httr2` request object reused to request signed links.

## Value

Path to the downloaded asset or `NULL` if the download fails.
