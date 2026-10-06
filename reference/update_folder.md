# Replace a Folder in Databrary Volume (PUT)

Sends a PUT request to fully replace a folder's writable fields. `name`
is required (non-empty); other fields are optional and default to
server-side values when omitted (the underlying serializer marks them
`required=FALSE`). Use
[`patch_folder`](https://databrary.github.io/databraryr/reference/patch_folder.md)
for partial updates when you don't want full-replacement semantics.

## Usage

``` r
update_folder(
  vol_id = 1,
  folder_id,
  name,
  release_level = NULL,
  source_date = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  Target volume number. Must be a positive integer.

- folder_id:

  Numeric folder identifier. Must be a positive integer.

- name:

  New folder name. Required, non-empty after trim.

- release_level:

  Optional release level (e.g. `"PRIVATE"`, `"SHARED"`, `"EXCERPTS"`,
  `"PUBLIC"`).

- source_date:

  Optional folder date. A length-1 `Date` object or ISO `"YYYY-MM-DD"`
  string.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the updated folder's metadata (same shape as
[`get_folder_by_id`](https://databrary.github.io/databraryr/reference/get_folder_by_id.md)),
or `NULL` if the update fails.

## See also

[`patch_folder`](https://databrary.github.io/databraryr/reference/patch_folder.md),
[`create_folder`](https://databrary.github.io/databraryr/reference/create_folder.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
update_folder(vol_id = 1, folder_id = 42, name = "Replacement name")
} # }
# }
```
