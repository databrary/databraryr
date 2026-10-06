# Bulk Create Folders in a Databrary Volume

Create many folders sequentially. `input` in the result tibble is the
folder name for that row (after trimming). `folder_names` must be
unique. Optional fields are recycled: length 1 or same length as
`folder_names`.

## Usage

``` r
bulk_create_folders(
  vol_id = 1,
  folder_names,
  release_level = NULL,
  source_date = NULL,
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

- folder_names:

  Non-empty character vector of folder names (trimmed; none may be empty
  after trimming).

- release_level:

  Optional character vector (length 1 or `length(folder_names)`), passed
  per row to
  [`create_folder`](https://databrary.github.io/databraryr/reference/create_folder.md).

- source_date:

  Optional `Date`, ISO string, or vector thereof (length 1 or
  `length(folder_names)`).

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
[`bulk_upload_files`](https://databrary.github.io/databraryr/reference/bulk_upload_files.md),
with `input` equal to the trimmed folder name for each row.

## See also

[`create_folder`](https://databrary.github.io/databraryr/reference/create_folder.md),
[`resume_bulk`](https://databrary.github.io/databraryr/reference/resume_bulk.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
bulk_create_folders(vol_id = 1, folder_names = c("A", "B", "C"))
} # }
# }
```
