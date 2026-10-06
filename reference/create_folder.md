# Create Folder in Databrary Volume

Create a new folder in a Databrary volume. `name` is required and must
be non-empty. Folders group files together but, unlike sessions, do not
carry structured date / default-record metadata.

## Usage

``` r
create_folder(
  vol_id = 1,
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

- name:

  Display name for the folder. Required, non-empty after trim.

- release_level:

  Optional release level for the folder (e.g. `"PRIVATE"`, `"SHARED"`,
  `"EXCERPTS"`, `"PUBLIC"`). The server validates the choice.

- source_date:

  Optional folder date. A length-1 `Date` object or ISO `"YYYY-MM-DD"`
  string.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list with the created folder's metadata (same shape as
[`get_folder_by_id`](https://databrary.github.io/databraryr/reference/get_folder_by_id.md)),
or `NULL` if creation fails.

## See also

[`update_folder`](https://databrary.github.io/databraryr/reference/update_folder.md),
[`patch_folder`](https://databrary.github.io/databraryr/reference/patch_folder.md),
[`delete_folder`](https://databrary.github.io/databraryr/reference/delete_folder.md)

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Minimal folder
create_folder(vol_id = 1, name = "Stimuli")

# Folder with a release level and date
create_folder(
  vol_id = 1,
  name = "Stimuli",
  release_level = "SHARED",
  source_date = as.Date("2024-03-15")
)
} # }
# }
```
