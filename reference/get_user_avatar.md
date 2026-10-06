# Get User Avatar

Download a user's avatar image from Databrary. Returns raw bytes if no
destination path is specified, or saves to disk and returns the file
path.

## Usage

``` r
get_user_avatar(user_id, dest_path = NULL, vb = options::opt("vb"), rq = NULL)
```

## Arguments

- user_id:

  Numeric. The ID of the user whose avatar to download.

- dest_path:

  Optional character string specifying where to save the avatar. Can be
  either a file path or a directory. If a directory is provided, the
  filename will be automatically determined from the response headers or
  will default to "user\_\_avatar.jpg". If `NULL` (the default), the
  function returns raw bytes instead of saving to disk.

- vb:

  Show verbose messages. (Defaults to `FALSE`, overwritable using option
  'databraryr.vb' or environment variable 'R_DATABRARYR_VB')

- rq:

  An `httr2` request object. Defaults to `NULL`.

## Value

If `dest_path` is `NULL`, returns raw bytes. If `dest_path` is
specified, returns the full path where the avatar was saved. Returns
`NULL` if the user has no avatar or if an error occurs.

## Examples

``` r
# \donttest{
if (FALSE) { # \dontrun{
# Get avatar as raw bytes
avatar_bytes <- get_user_avatar(user_id = 5)

# Save avatar to specific file
get_user_avatar(user_id = 5, dest_path = "avatar.jpg")

# Save avatar to directory (filename auto-determined)
get_user_avatar(user_id = 5, dest_path = "~/avatars/")

# With verbose output
get_user_avatar(user_id = 5, dest_path = "avatar.jpg", vb = TRUE)
} # }
# }
```
