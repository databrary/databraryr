# Download Institution Avatar Image

Download an institution's avatar image from Databrary. The image can be
saved to a file or returned as raw bytes for further processing.

## Usage

``` r
get_institution_avatar(
  institution_id = 1,
  dest_path = NULL,
  vb = options::opt("vb"),
  rq = NULL
)
```

## Arguments

- institution_id:

  Numeric institution identifier. Must be a positive integer.

- dest_path:

  Optional character string specifying the destination file path or
  directory where the avatar should be saved. If a directory is
  provided, the filename will be determined from the response headers or
  will default to `institution_<id>_avatar.jpg`. If `NULL` (the
  default), the raw image bytes are returned instead of being saved to
  disk.

- vb:

  Show verbose feedback. Defaults to `options::opt("vb")`.

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

If `dest_path` is provided, returns the full path to the saved file
(character string). If `dest_path` is `NULL`, returns the raw image
bytes. Returns `NULL` if the avatar is not found or inaccessible.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Download avatar as raw bytes
avatar_bytes <- get_institution_avatar(institution_id = 1)

# Download and save avatar to specific file
avatar_path <- get_institution_avatar(
  institution_id = 1,
  dest_path = "institution_1_avatar.jpg"
)

# Download and save to directory (filename auto-determined)
avatar_path <- get_institution_avatar(
  institution_id = 1,
  dest_path = "avatars/"
)

# With verbose output
get_institution_avatar(institution_id = 1, vb = TRUE)
} # }
# }
```
