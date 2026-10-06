# Replace a Session File's Metadata (PUT)

Sends a PUT request to fully replace a session file's writable metadata
fields. `name` is required (non-empty); other fields are optional and
default to server-side values when omitted. Use
[`patch_session_file`](https://databrary.github.io/databraryr/reference/patch_session_file.md)
for partial updates when you don't want full-replacement semantics.

## Usage

``` r
update_session_file(
  vol_id = 1,
  session_id,
  file_id,
  name,
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

  New file name. Required, non-empty after trim.

- release_level:

  Optional release level (e.g. `"PRIVATE"`, `"SHARED"`, `"EXCERPTS"`,
  `"PUBLIC"`).

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
or `NULL` if the update fails.

## See also

[`patch_session_file`](https://databrary.github.io/databraryr/reference/patch_session_file.md),
[`delete_session_file`](https://databrary.github.io/databraryr/reference/delete_session_file.md),
[`get_session_file`](https://databrary.github.io/databraryr/reference/get_session_file.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Rename a file via PUT
update_session_file(
  vol_id = 1,
  session_id = 42,
  file_id = 99,
  name = "Replacement name.mp4"
)
} # }
# }
```
