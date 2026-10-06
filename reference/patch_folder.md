# Partially Update a Folder in Databrary Volume

Sends a PATCH request to update selected fields of an existing folder.
Only provided arguments are sent; omitted fields are left unchanged on
the server. The server rejects blanking out a previously non-empty
`name`.

## Usage

``` r
patch_folder(
  vol_id = 1,
  folder_id,
  name = NULL,
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

  Optional new folder name. If provided, must be a non-empty length-1
  string.

- release_level:

  Optional release level (e.g. `"PRIVATE"`, `"SHARED"`, `"EXCERPTS"`,
  `"PUBLIC"`). Server validates the choice.

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
or `NULL` if the update fails or no fields were provided.

## See also

[`update_folder`](https://databrary.github.io/databraryr/reference/update_folder.md),
[`create_folder`](https://databrary.github.io/databraryr/reference/create_folder.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
patch_folder(vol_id = 1, folder_id = 42, name = "Renamed folder")
} # }
# }
```
