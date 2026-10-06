# Partially Update a Session File's Metadata

Sends a PATCH request to update selected metadata fields of an existing
session file. Only provided arguments are sent; omitted fields are left
unchanged on the server.

## Usage

``` r
patch_session_file(
  vol_id = 1,
  session_id,
  file_id,
  name = NULL,
  release_level = NULL,
  source_date = NULL,
  date = NULL,
  date_precision = NULL,
  is_estimated = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- session_id:

  Numeric session identifier. Must be a positive integer.

- file_id:

  Numeric file identifier. Must be a positive integer.

- name:

  Optional new file name. If provided, must be a non-empty length-1
  string.

- release_level:

  Optional release level (e.g. `"PRIVATE"`, `"SHARED"`, `"EXCERPTS"`,
  `"PUBLIC"`). Server validates the choice.

- source_date:

  Optional file date. A length-1 `Date` object or ISO `"YYYY-MM-DD"`
  string. Mutually exclusive with `date`.

- date:

  Optional structured date list with named fields `year`, `month`,
  `day`, and optional `is_estimated` (logical). Mutually exclusive with
  `source_date`.

- date_precision:

  Optional precision for `date`: e.g. `"FULL"`, `"YEAR"`.

- is_estimated:

  Optional logical flag indicating whether the date is estimated.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the updated file's metadata (same shape as
[`get_session_file`](https://databrary.github.io/databraryr/reference/get_session_file.md)),
or `NULL` if the update fails or no fields were provided.

## See also

[`update_session_file`](https://databrary.github.io/databraryr/reference/update_session_file.md),
[`delete_session_file`](https://databrary.github.io/databraryr/reference/delete_session_file.md),
[`get_session_file`](https://databrary.github.io/databraryr/reference/get_session_file.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Rename a file
patch_session_file(
  vol_id = 1, session_id = 42, file_id = 99, name = "Renamed.mp4"
)

# Update the date and precision
patch_session_file(
  vol_id = 1,
  session_id = 42,
  file_id = 99,
  date = list(year = 2024, month = 3, day = 15),
  date_precision = "FULL"
)
} # }
# }
```
