# Get Session File Data From A Databrary Volume

Databrary volumes have folders where study or collection-wide files can
be stored and shared. `get_folder_file()` returns metadata about
specific files stored in a volume folder.

## Usage

``` r
get_folder_file(
  vol_id = 1,
  folder_id = 9807,
  file_id = 1,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- vol_id:

  An integer indicating the volume identifier. Default is 1.

- folder_id:

  An integer indicating a valid folder identifier linked to a volume.
  Default value is 9807, the materials folder for volume 1.

- file_id:

  An integer indicating the file identifier. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An httr2 request object.

## Value

A JSON blob with the file data. If the user has previously logged in to
Databrary via
[`login_db()`](https://databrary.github.io/databraryr/reference/login_db.md),
then files that have restricted access can be downloaded, subject to the
sharing release levels on those files.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
get_folder_file() # Data about file_id 1 from folder_id 9807 in Volume 1.
get_folder_file(vol_id = 2, folder_id = 9819, file_id = 16) # A PDF from Volume 2.
} # }
# }
```
