# Bulk Rename Folders in a Databrary Volume

Rename many folders sequentially via
[`patch_folder`](https://databrary.github.io/databraryr/reference/patch_folder.md)
(`name` only). `folder_ids` must be unique. With
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md),
pass `new_names` aligned to incomplete rows.

## Usage

``` r
bulk_rename_folders(
  vol_id = 1,
  folder_ids,
  new_names,
  vb = options::opt("vb"),
  rq = NULL,
  on_error = c("stop", "collect"),
  max_retries = 0L,
  retry_delay = 0
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- folder_ids:

  Numeric vector of folder identifiers (unique positive integers).

- new_names:

  Character vector of new names, same length as `folder_ids`.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

- on_error:

  `"stop"` or `"collect"`; see
  [`bulk_upload_files`](https://databrary.github.io/databraryr/reference/bulk_upload_files.md).

- max_retries:

  Non-negative integer; extra attempts per input after the first
  failure.

- retry_delay:

  Seconds between retries.

## Value

A `tibble` as documented in
[`bulk_upload_files`](https://databrary.github.io/databraryr/reference/bulk_upload_files.md);
`input` is each folder id.

## See also

[`patch_folder`](https://databrary.github.io/databraryr/reference/patch_folder.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
bulk_rename_folders(
  vol_id = 1,
  folder_ids = c(11, 12),
  new_names = c("Stimuli", "Protocols")
)
} # }
# }
```
