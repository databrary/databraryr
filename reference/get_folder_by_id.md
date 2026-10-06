# Get Folder Metadata From a Databrary Volume

Get Folder Metadata From a Databrary Volume

## Usage

``` r
get_folder_by_id(
  folder_id = 9807,
  vol_id = 1,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- folder_id:

  Folder identifier within the specified volume. Default is 9807, the
  Materials folder for Volume 1.

- vol_id:

  Volume identifier containing the folder. Default is 1.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

A list representing the folder metadata, or `NULL` when the folder
cannot be accessed.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
get_folder_by_id() # Default folder in volume 1
} # }
# }
```
